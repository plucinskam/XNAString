#' Create set of functions and methods to calculate dinucleotide frequency in
#' base, sugar and backbone slots
#'
#' @param unique_sets string vector of double letters -these letters pose column names
#' @param seq string (or character) - frequency is calculated for
#'                                   this string
#' @param as.prob logical - if TRUE frequency returned as probability of
#'                          occurrence
#'
#' @return numeric - named numeric vector
#' @importFrom stringr str_sub
#'
#' @examples seqDinucleotideFrequency(c("AB", "BA", "CD"), "ABABAB", as.prob = FALSE)
#' seqDinucleotideFrequency(c("GC", "CG", "CC"), "GCCG", as.prob = FALSE)
#' @export
seqDinucleotideFrequency <- function(unique_sets, seq, as.prob) {
  # split sequence to double letters
  if (!is.na(seq)) {
    n <- nchar(seq)
    seq <- stringr::str_sub(seq, seq(1, n - 1), seq(2, n))
  } else {
    seq <- ""
  }

  # return frequency
  freq <- base::table(seq)
  if (as.prob == TRUE) {
    freq <- base::prop.table(freq)
  }

  # table function returns frequency for double letters existing in sequence,
  # let's extend it to the possible double letters in unique_letters
  extended_freq <- vapply(unique_sets, function(letter) {
    freq[letter]
  }, 1.0)
  # if na -> 0 occurence
  extended_freq[is.na(extended_freq)] <- 0
  names(extended_freq) <- unique_sets

  return(extended_freq)
}


#' seqVectorDinucleotideFrequency function calculates frequency for strings vector
#'
#' @param unique_sets string vector of double letters -these letters pose column names
#' @param seq_vec vector of strings (or characters) - frequency will be
#'                calculated for this vector
#' @param as.prob logical - if TRUE frequency returned as probability of
#'                          occurence
#'
#' @return matrix - each row denotes frequency for a specific string of vector
#'
#' @examples seqVectorDinucleotideFrequency(c("AB", "BA", "CD"), c("ABABAB", "ABABCD"), as.prob = FALSE)
#' @export
seqVectorDinucleotideFrequency <-
  function(unique_sets, seq_vec, as.prob) {
    # # create matrix M to store frequency for each element in seq_vec
    M <-
      matrix(,
        nrow = length(seq_vec),
        ncol = length(unique_sets)
      )

    M <- sapply(seq(1, length(seq_vec)), function(i) {
      M[i, ] <- seqDinucleotideFrequency(unique_sets, seq_vec[i], as.prob)
    })
    M <- t(M)
    return(M)
  }


#' XNADinucleotideFrequencyFun returns double letters frequency for a given
#' object in base, sugar or backbone slot
#'
#' @rdname dinucleotideFrequency
#' @name dinucleotideFrequency
#'
#' @importFrom formattable formattable
#' 
#' @examples 
#' xnastring_obj <- XNAString(
#'  name = "b",
#'  base = c("GGEG"),
#'  sugar = c("FFOO"),
#'  dictionary = my_dic
#' )
#' XNAString::XNADinucleotideFrequency(
#'  obj = xnastring_obj,
#'  slot = "base",
#'  matrix_nbr = 1
#' )
#' @export
XNADinucleotideFrequencyFun <-
  function(obj,
           slot,
           double_letters = NA,
           matrix_nbr = 1,
           as.prob = FALSE,
           base_only = FALSE) {
    (is(obj, "XNAString") | is(obj, "XNAStringSet")) ||
      stop("An object must be of XNAString or XNAStringSet class")

    !(base_only == TRUE & !all(is.na(double_letters))) ||
      stop("If base_only is TRUE, double_letters argument must be ommited")

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

    # if double_letters missing, check on the possible double_letters in dictionary
    if (any(is.na(double_letters))) {
      dictionary <- obj@objects[[1]]@dictionary
      poss_letters <-
        sort(unique(dictionary[dictionary$type == slot][["symbol"]]))
      double_letters <- c(outer(poss_letters, poss_letters, paste0))
    }

    # if base_only TRUE and A,C,G, or T not in dictionary -> warning
    base_double_letters <-
      c(
        "AA",
        "CA",
        "GA",
        "TA",
        "AC",
        "CC",
        "GC",
        "TC",
        "AG",
        "CG",
        "GG",
        "TG",
        "AT",
        "CT",
        "GT",
        "TT"
      )
    !(base_only == TRUE &
      !all(base_double_letters %in% double_letters)) ||
      stop(
        "base_only parameter set as TRUE, but the objet's dictionary or
           double_letters parameter does not include even one combination of base
           letters: A, C, G or T"
      )

    eval(parse(
      text = paste(
        "freq <- lapply(obj_dt$",
        slot,
        ", function(",
        slot,
        ") {seqVectorDinucleotideFrequency(double_letters ,",
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
        colnames(freq)[!colnames(freq) %in% c(
          "AA",
          "CA",
          "GA",
          "TA",
          "AC",
          "CC",
          "GC",
          "TC",
          "AG",
          "CG",
          "GG",
          "TG",
          "AT",
          "CT",
          "GT",
          "TT"
        )]
      base_letters_freq <- freq[, base_double_letters]
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






#' XNADinucleotideFrequency method returns dinucleotide frequency for a given object.
#' It works for 3 slots: base, sugar and backbone. If matrix_nbr equals 1,
#' dinucleotide frequency for the first elements in the slot is returned.
#' Double letters can be given as argument, otherwise unique double letters in
#' object's dictionary are in use.
#' @param obj XNAString or XNAStringSet class
#' @param slot string (slot name: base, sugar or backbone)
#' @param double_letters string (or string vector) - double letters
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
#' @include xnaStringClass.R
#' @include xnaStringSetClass.R
#' @rdname dinucleotideFrequency
#' @export
setGeneric("XNADinucleotideFrequency",
  signature = "obj",
  function(obj,
           slot,
           double_letters = NA,
           matrix_nbr = 1,
           as.prob = FALSE,
           base_only = FALSE,
           ...) {
    standardGeneric("XNADinucleotideFrequency")
  }
)

#' @rdname dinucleotideFrequency
setMethod(
  "XNADinucleotideFrequency", c("XNAString"),
  function(obj,
           slot,
           double_letters = NA,
           matrix_nbr = 1,
           as.prob = FALSE,
           base_only = FALSE) {
    XNADinucleotideFrequencyFun(
      obj,
      slot,
      double_letters,
      matrix_nbr,
      as.prob,
      base_only
    )
  }
)

#' @rdname dinucleotideFrequency
setMethod(
  "XNADinucleotideFrequency", c("XNAStringSet"),
  function(obj,
           slot,
           double_letters = NA,
           matrix_nbr = 1,
           as.prob = FALSE,
           base_only = FALSE) {
    XNADinucleotideFrequencyFun(
      obj,
      slot,
      double_letters,
      matrix_nbr,
      as.prob,
      base_only
    )
  }
)
