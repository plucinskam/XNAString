#' Reverse complement sequence based on dictionary
#' @importMethodsFrom  Biostrings reverseComplement
#'
#' @param obj XNAString object
#'
#' @return string with reverse complement sequence
#' @importFrom stringi stri_reverse
#'
reverseComplementFun <-
  function(obj) {
    iupac_dict <-
      data.table::data.table(
        symbol = c("W", "S", "M", "K", "R", "Y", "B", "D", "H", "V", "N"),
        bases = list(
          list("A", "T"),
          list("G", "C"),
          list("A", "C"),
          list("G", "T"),
          list("A", "G"),
          list("C", "T"),
          list("C", "G", "T"),
          list("A", "G", "T"),
          list("A", "C", "T"),
          list("A", "C", "G"),
          list("A", "C", "G", "T")
        )
      )
    base <- obj@base

    # BASE CHARACTER
    if (class(base)[[1]] == "character") {
      dictionary <- obj@compl_dictionary

      target_dict <- paste(dictionary[["target"]], collapse = "")
      base_dict <- paste(dictionary[["base"]], collapse = "")

      complement <- chartr(base_dict, target_dict, base)
      reverse_complement <- stringi::stri_reverse(complement)

      iupac_in_string_idx <-
        iupac_dict$symbol %in% strsplit(reverse_complement, split = "")[[1]]
      iupac_in_string <- iupac_dict[iupac_in_string_idx, ]

      reverse_complement <-
        generateAllCombinations(str = reverse_complement, iupac_dict = iupac_in_string)

      # BASE DNASTRING OR RNASTRING
    } else if (class(base)[[1]] %in% c("DNAString", "DNAStringSet", "RNAString", "RNAStringSet")) {
      reverse_complement <- Biostrings::reverseComplement(base)
    }

    return(reverse_complement)
  }

#' Generate all string combinations based on IUPAC dictionary
#'
#' @param str character string of bases with IUPAC symbols
#' @param iupac_dict data.table mapping IUPAC symbols to bases
#' @noRd
generateAllCombinations <- function(str,
                                    iupac_dict) {
  splited_str <- as.list(strsplit(str, split = "")[[1]])

  symbol <- NULL
  any(colnames(iupac_dict) == "symbol") || stop("Iupac dict must include symbol column.")

  for (iupac_symbol in iupac_dict[["symbol"]]) {
    coded_bases <- iupac_dict[symbol == iupac_symbol][["bases"]]
    splited_str[splited_str == iupac_symbol] <- coded_bases
  }

  all_combinations_df <- expand.grid(splited_str)

  all_combinations <- apply(all_combinations_df, 1, function(x) {
    paste0(x, collapse = "")
  })

  return(all_combinations)
}

#' Reverse complement sequence based on dictionary
#' @param obj XNAString object
#' @param ... optional arguments to generic function to support additional methods
#'
#' @return string with reverse complement sequence
#' @rdname reverseComplement
#'
#' @export
#' @examples
#' my_dic <- data.table::data.table(
#'   type = c(
#'     rep("base", 3),
#'     rep("sugar", 2),
#'     rep("backbone", 3)
#'   ),
#'   symbol = c("G", "E", "A", "F", "O", "S", "B", "X")
#' )
#' obj <- XNAString(
#'   name = "b",
#'   base = "GGE",
#'   sugar = "FFO",
#'   dictionary = my_dic
#' )
#' XNAReverseComplement(obj)
setGeneric("XNAReverseComplement",
  signature = "obj",
  function(obj,
           ...) {
    standardGeneric("XNAReverseComplement")
  }
)

#' @rdname reverseComplement
setMethod(
  "XNAReverseComplement", c("XNAString"),
  function(obj) {
    reverseComplementFun(obj)
  }
)
