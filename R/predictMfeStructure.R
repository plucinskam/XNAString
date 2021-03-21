#' Prediction of MFE structure with ViennaRNA package
#'
#' This function is a wrapper for RNAfold from ViennaRNA package.
#'
#' @param obj XNAString object
#' @param ... optional arguments to generic function to support
#'  additional methods
#'
#' @rawNamespace useDynLib(XNAString)
#' @import Rcpp
#' @return list with mfe and structure
#' @rdname mfeStructure
#' @name predictMfeStructure
#'
predictMfeStructureFun <- function(obj) {
  # for now DNAStringSet and RNASTringSet not allowed
  
  any((is(obj@base, "DNAString") | is(obj@base, "RNAString")) |
        (is(obj@base, "character") & length(obj@base) == 1)) ||
    stop("sequence parameter must 1-element character vector, 
         DNAString or RNAString type")
  
  if (class(obj@base)[[1]] == "character") {
    # check if each base has non-empty corresponding compl_target
    if (all(strsplit(obj@base, "")[[1]] %in% c("A", "C", "G", "T", "U"))) {
      base <- obj@base
    } else {
      # if other letters than from basic complementary_bases dictionary
      base <- changeBase(obj@compl_dictionary, obj@base)
    }
    
    if (base != "") {
      prediction_results <- RNAfold_MFE(base)
    } else {
      prediction_results <- ""
    }
  } else {
    prediction_results <- RNAfold_MFE(as.character(obj@base))
  }
  
  result <- list(structure = prediction_results[1],
                 mfe = as.numeric(prediction_results[2]))
  
  return(result)
}



#' @rdname mfeStructure
#' @export
setGeneric("predictMfeStructure",
           signature = "obj",
           function(obj,
                    ...) {
             standardGeneric("predictMfeStructure")
           })

#' @rdname mfeStructure
#' @examples
#' obj1 <- XNAString(
#'   base = "ATCG",
#'   sugar = "FODD",
#'   conjugate3 = "TAG"
#' )
#' predictMfeStructure(obj1)
setMethod("predictMfeStructure", c("XNAString"),
          function(obj) {
            predictMfeStructureFun(obj)
          })
