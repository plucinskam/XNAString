#' Prediction of MFE structure with ViennaRNA package
#'
#' This function is a wrapper for RNAfold from ViennaRNA package.
#'
#' @param obj XNAString object
#' @param ... optional arguments to generic function to support additional
#' methods
#'
#' @rawNamespace useDynLib(XNAString)
#' @import Rcpp
#'
#' @rdname mfeStructure
#' @name predictMfeStructure
#'
predictMfeStructureFun <- function(obj) {
  # for now DNAStringSet and RNASTringSet not allowed
  any((is(base(obj), "DNAString") |  is(base(obj), "RNAString")) |
        (is(base(obj), 'character') & length(base(obj)) == 1)) ||
    stop("sequence parameter must 1-element character vector, ",
         "DNAString or RNAString type")
  
  if (class(base(obj))[[1]] == 'character') {
    # check if each base has non-empty corresponding compl_target
    # if change_base(obj$compl_dictionary, obj@base) empty , then predict
    # results ''
    if (all(strsplit(base(obj), "")[[1]] %in% c('A', 'C', 'G', 'T', 'U'))) {
      base <- base(obj)
    } else {
      # if other letters than from basic complementary_bases dictionary
      base <- changeBase(compl_dictionary(obj), base(obj))
    }
    if (base != "") {
      prediction_results <- RNAfold_MFE(base)
    } else {
      prediction_results <- ""
    }
  } else {
    prediction_results <- RNAfold_MFE(as.character(base(obj)))
  }
  result <- list(structure = prediction_results[1],
                 mfe = as.numeric(prediction_results[2]))
  return(result)
}



#' @rdname mfeStructure
#' @examples
#' obj1 <- XNAString(
#'   base = "ATCG",
#'   sugar = "FODD",
#'   conjugate3 = "TAG"
#' )
#' predictMfeStructure(obj1)
#'
#' @return character, secondary structure in dot-bracket notation
#' @export
setGeneric("predictMfeStructure",
           signature = "obj",
           function(obj,
                    ...) {
             standardGeneric("predictMfeStructure")
           })

#' @rdname mfeStructure
setMethod("predictMfeStructure", c("XNAString"),
          function(obj) {
            predictMfeStructureFun(obj)
          })
#' @examples
#'  obj1 <- XNAString(base = 'ATCG',
#'                    sugar = 'FODD',
#'                    conjugate3 = 'TAG')
#'  predictMfeStructure(obj1)
