#' Compute Minimum Free Energy (MFE), and a corresponding secondary structure
#' for two dimerized RNA sequences.
#'
#' This function is a wrapper for RNAcofold from ViennaRNA package.
#'
#' @param obj XNAString object
#' @param ... optional arguments to generic function to support additional methods
#' @return list (structure and mfe)
#'
#' @rawNamespace useDynLib(XNAString)
#' @import Rcpp
#'
#' @rdname duplexStructure
#' @name predictDuplexStructure
#'
#'
predictDuplexStructureFun <- function(obj) {
  base_sl <- obj@base
  # for now DNAStringSet and RNASTringSet not allowed
  ((any(
    class(base_sl)[[1]] == c("DNAStringSet", "RNAStringSet")
  ) & length(base_sl) <= 2) |
    (any(
      class(base_sl)[[1]] == c("DNAString", "RNAString")
    )) |
    (class(base_sl)[[1]] == "character" &
      length(base_sl) <= 2)) ||
    stop(
      "Base must either 2-elements character vector / DNAStringSet / RNAStringSet or 1-element character vector / DNAString / RNAString."
    )

  # find length of first sequence (needed to add ampersand in between)
  seq1_length <- nchar(as.character(base_sl)[1])
  # if 2nd base empty - the same length as 1st base
  seq2_length <-
    if (is.na(nchar(as.character(base_sl)[2]))) {
      seq1_length
    } else {
      nchar(as.character(base_sl)[2])
    }

  if (class(base_sl)[[1]] == "character") {
    if (all(unlist(strsplit(base_sl, "")) %in% c("A", "C", "G", "T", "U"))) {
      base_new <- base_sl
    } else {
      # if other letters than from basic complementary_bases dictionary
      base_new <- changeBase(obj@compl_dictionary, base_sl)
    }
    # if there is lacking compl_target input in compl_dict, then base = ''
    if (all(base_new != "")) {
      prediction_results <-
        RNAcofold_MFE(paste(if (length(base_new) == 1) {
          c(base_new, base_new)
        } else {
          base_new
        }, collapse = "&"))
    } else {
      prediction_results <- ""
    }
  } else {
    base_sl <- as.character(base_sl)
    prediction_results <-
      RNAcofold_MFE(paste(if (length(base_sl) == 1) {
        c(base_sl, base_sl)
      } else {
        base_sl
      }, collapse = "&"))
  }

  result <- list(
    # structure = prediction_results[1],
    structure = paste0(
      stri_sub(prediction_results[1], 1, seq1_length),
      "&",
      stri_sub(
        prediction_results[1],
        seq1_length + 1,
        seq1_length + seq2_length
      ),
      collapse = ""
    ),
    mfe = as.numeric(prediction_results[2])
  )

  return(result)
}



#' @rdname duplexStructure
#' @export
setGeneric("predictDuplexStructure",
  signature = "obj",
  function(obj,
           ...) {
    standardGeneric("predictDuplexStructure")
  }
)

#' @rdname duplexStructure
#' @examples
#' obj1 <- XNAString(
#'   base = "ATCG",
#'   sugar = "FODD",
#'   conjugate3 = "TAG"
#' )
#' predictDuplexStructure(obj1)
setMethod(
  "predictDuplexStructure", c("XNAString"),
  function(obj) {
    predictDuplexStructureFun(obj)
  }
)
