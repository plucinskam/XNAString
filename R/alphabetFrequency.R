#' Create set of functions and methods to calculate alphabet frequency in
#' base, sugar and backbone slots
#'
#' @param unique_letters string (or character) - these letters pose column names
#' @param seq string (or character) - frequency is calculated for
#'                                   this string
#' @param as.prob logical - if TRUE frequency returned as probability of
#'                          occurrence
#'
#' @return numeric - named numeric vector
#'
#' @examples seqAlphabetFrequency(c("A", "B", "C"), c("AABA"), as.prob = FALSE)
#' @export
seqAlphabetFrequency <- function(unique_letters, seq, as.prob) {
  # split sequence to single characters
  seq <- strsplit(seq, "")[[1]]

  # return frequency
  freq <- base::table(seq)
  if (as.prob == TRUE) {
    freq <- base::prop.table(freq)
  }

  # table function returns frequency for letters existing in sequence,
  # let's extend it to the possible letters in unique_letters
  extended_freq <- vapply(unique_letters, function(letter) {
    freq[letter]
  }, 1.0)
  # if na -> 0 occurence
  extended_freq[is.na(extended_freq)] <- 0
  names(extended_freq) <- unique_letters

  return(extended_freq)
}




#' seqVectorAlphabetFrequency function calculates frequency for strings vector
#'
#' @param unique_letters string (or character) - these letters pose column names
#' @param seq_vec vector of strings (or characters) - frequency will be
#'                calculated for this vector
#' @param as.prob logical - if TRUE frequency returned as probability of
#'                          occurence
#'
#' @return matrix - each row denotes frequency for a specific string of vector
#'
#' @examples seqVectorAlphabetFrequency(c("A", "B", "C"),
#'   c("AABA", "BBBCCC"),
#'   as.prob = FALSE
#' )
#' @export
seqVectorAlphabetFrequency <-
  function(unique_letters, seq_vec, as.prob) {
    # # create matrix M to store frequency for each element in seq_vec
    M <-
      matrix(,
        nrow = length(seq_vec),
        ncol = length(unique_letters)
      )

    M <- sapply(seq(1, length(seq_vec)), function(i) {
      M[i, ] <- seqAlphabetFrequency(unique_letters, seq_vec[i], as.prob)
    })
    M <- t(M)
    return(M)
  }


#' XNAAlphabetFrequency returns letters frequency for a given object in base,
#' sugar or backbone slot
#'
#' @rdname alphabetFrequency
#' @name alphabetFrequency
#'
#' @importFrom formattable formattable
#' @export
XNAAlphabetFrequencyFun <-
  function(obj,
           slot,
           letters = NA,
           matrix_nbr = 1,
           as.prob = FALSE,
           base_only = FALSE) {
    (is(obj, "XNAString") | is(obj, "XNAStringSet")) ||
      stop("An object must be of XNAString or XNAStringSet class")

    # ifelse(base_only == TRUE & !is.na(letters), FALSE, TRUE) ||
    !(base_only == TRUE & !all(is.na(letters))) ||
      stop("If base_only is TRUE, letters argument must be ommited")

    matrix_nbr %in% c(1, 2) ||
      stop("The matrix_nbr must be either 1 or 2")

    # base can be DNAString or DNAStringSet, change it to character with base methods
    # on the right side, base getter gets base slot and changes to character
    # on the left, base setter changes the base slot
    if (class(obj)[[1]] == "XNAString") {
      base(obj) <- base(obj)
    } else {
      base(obj, 1) <- base(obj, 1)
      if (!any(base(obj, 2) == "")) {
        base(obj, 2) <- base(obj, 2)
      }
    }

    # if object is "XNAString", change it to XNAStringSet
    if (class(obj)[[1]] == "XNAString") {
      obj <- XNAString2XNAStringSet(obj)
    }

    obj_dt <- set2Dt(obj, slots = c(slot))

    # if letters missing, check on the possible letters in dictionary
    if (any(is.na(letters))) {
      dictionary <- obj@objects[[1]]@dictionary
      letters <-
        sort(unique(dictionary[dictionary$type == slot][["symbol"]]))
    }

    # if base_only TRUE and A,C,G, or T not in dictionary -> warning
    base_letters <- c("A", "C", "G", "T")
    !(base_only == TRUE & !all(base_letters %in% letters)) ||
      stop("base_only parameter set as TRUE, but the objet's dictionary or
           letters parameter does not include at least one of the base
           letters: A, C, G or T")

    eval(parse(
      text = paste(
        "freq <- lapply(obj_dt$",
        slot,
        ", function(",
        slot,
        ") {seqVectorAlphabetFrequency(letters ,",
        slot,
        "[matrix_nbr], as.prob)})",
        sep = ""
      )
    ))

    cols <- colnames(freq[[1]])
    freq <-
      matrix(unlist(freq),
        nrow = length(freq),
        byrow = TRUE
      )
    colnames(freq) <- cols

    # if base_only TRUE, frequency for A, C, G, T and other
    if (base_only == TRUE) {
      other_letters <-
        colnames(freq)[!colnames(freq) %in% c("A", "C", "G", "T")]
      base_letters_freq <- freq[, base_letters]
      if (nrow(freq) > 1) {
        other_letter_freq <- rowSums(freq[, other_letters])
        freq <- cbind(base_letters_freq, other_letter_freq)
        colnames(freq)[ncol(freq)] <- "other"
      } else {
        other_letter_freq <- sum(freq[, other_letters])
        freq <- c(base_letters_freq, other_letter_freq)
        names(freq)[length(freq)] <- "other"
      }
    }

    freq <- formattable(freq, digits = 2, format = "f")

    return(freq)
  }

#' XNAAlphabetFrequency method returns alphabet frequency for a given object.
#' It works for 3 slots: base, sugar and backbone. If matrix_nbr equals 1,
#' alphabet frequency for the first elements in the slot is returned.
#' Letters can be given as argument, otherwise unique letters in
#' object's dictionary are in use.
#' @param obj XNAString or XNAStringSet class
#' @param slot string (slot name: base, sugar or backbone)
#' @param letters character (or character vector)
#' @param matrix_nbr numeric (1 or 2, if 1 - first slot's element is use,
#'                            if 2 - 2nd element in slot)
#' @param as.prob logical - if TRUE frequency returned as probability of
#'                          occurence
#' @param base_only logical - if TRUE, frequency checked for
#'                            'A', 'C', 'G', 'T', other
#' @param ... optional arguments to generic function to support additional methods
#'
#' @return matrix (frequency matrix for a given slot)
#'
#' @examples
#' xnastring_obj <- XNAString(
#'   name = "b",
#'   base = c("AACC", "GGEE"),
#'   sugar = c("FFOO", "OODD")
#' )
#' XNAAlphabetFrequency(obj = xnastring_obj, slot = "base")
#' XNAAlphabetFrequency(obj = xnastring_obj, slot = "base", as.prob = TRUE)
#' XNAAlphabetFrequency(obj = xnastring_obj, slot = "base", base_only = TRUE)
#' XNAAlphabetFrequency(obj = xnastring_obj, slot = "base", letters = c("A", "C"))
#' XNAAlphabetFrequency(obj = xnastring_obj, slot = "base", matrix_nbr = 2)
#'
#' xnastring_obj_2 <- XNAString(
#'   base = c("ATCG"),
#'   sugar = c("FODD"),
#'   backbone = c("SBB")
#' )
#' XNAStringSet_obj <- XNAStringSet(objects = list(
#'   xnastring_obj,
#'   xnastring_obj_2
#' ))
#' XNAAlphabetFrequency(XNAStringSet_obj, "sugar")
#' @include xnaStringClass.R
#' @include xnaStringSetClass.R
#' @rdname alphabetFrequency
#' @export
setGeneric("XNAAlphabetFrequency",
  signature = "obj",
  function(obj,
           slot,
           letters = NA,
           matrix_nbr = 1,
           as.prob = FALSE,
           base_only = FALSE,
           ...) {
    standardGeneric("XNAAlphabetFrequency")
  }
)

#' @rdname alphabetFrequency
setMethod(
  "XNAAlphabetFrequency", c("XNAString"),
  function(obj,
           slot,
           letters = NA,
           matrix_nbr = 1,
           as.prob = FALSE,
           base_only = FALSE) {
    XNAAlphabetFrequencyFun(
      obj,
      slot,
      letters,
      matrix_nbr,
      as.prob,
      base_only
    )
  }
)

#' @rdname alphabetFrequency
setMethod(
  "XNAAlphabetFrequency", c("XNAStringSet"),
  function(obj,
           slot,
           letters = NA,
           matrix_nbr = 1,
           as.prob = FALSE,
           base_only = FALSE) {
    XNAAlphabetFrequencyFun(
      obj,
      slot,
      letters,
      matrix_nbr,
      as.prob,
      base_only
    )
  }
)
