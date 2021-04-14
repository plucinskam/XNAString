#' @title setClassUnion definitions
#'
#' @description setClassUnion definitions used in XNAString class.
#' charOrDNAOrRNA consists of character, DNAString, RNAString, DNAStringSet,
#' RNAStringSet.
#' charOrDNA consists of character, DNAString, DNAStringSet
#'
#' @rdname xnastringClassUnions
#' @name xnastringClassUnions
setClassUnion(
  "charOrDNAOrRNA",
  c(
    "character",
    "DNAString",
    "RNAString",
    "DNAStringSet",
    "RNAStringSet"
  )
)

#' @rdname xnastringClassUnions
setClassUnion("charOrDNA",
              c("character",
                "DNAString",
                "DNAStringSet"))
