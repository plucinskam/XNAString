
#' XNAStringToHelmFun function takes XNAString object and translates
#' base, sugar and backbone to HELM notation
#' @param xnastring_obj XNAString object
#' @param dictionary HELM-symbol dictionary
#'
#' @return string (HELM notation)
#' @include xnaStringClass.R
#' @importFrom stats setNames
#'
#' @export
#' @examples
#' obj <- XNAString(
#'   base = "AAA",
#'   sugar = "DDD",
#'   backbone = "OO"
#' )
#' XNAStringToHelm(obj)
XNAStringToHelm <-
  function(xnastring_obj,
           dictionary = xna_dictionary) {
    base_dict <- dictionary[dictionary$type == "base", ]
    sugar_dict <- dictionary[dictionary$type == "sugar", ]
    backbone_dict <- dictionary[dictionary$type == "backbone", ]
    
    indx_base <-
      stats::setNames(as.character(base_dict$HELM), base_dict$symbol)
    indx_sugar <-
      stats::setNames(as.character(sugar_dict$HELM), sugar_dict$symbol)
    indx_backbone <-
      stats::setNames(as.character(backbone_dict$HELM), backbone_dict$symbol)
    
    base <- as.character(base(xnastring_obj))
    sugar <- as.character(sugar(xnastring_obj))
    backbone <- as.character(backbone(xnastring_obj))
    conjugate5 <- conjugate5(xnastring_obj)
    conjugate3 <- conjugate3(xnastring_obj)
    
    helms <- vapply(seq(1, length(base)), function(strand_nr) {
      base_trans <-
        vapply(strsplit(as.character(strsplit(base[strand_nr],
                                              "")[[1]]), ">"),
               function(x) {
                 paste(indx_base[x], collapse = ">")
               }, FUN.VALUE = 'character')
      
      sugar_trans <-
        vapply(strsplit(as.character(strsplit(sugar[strand_nr],
                                              "")[[1]]), ">"),
               function(x) {
                 paste(indx_sugar[x], collapse = ">")
               }, FUN.VALUE = 'character')
      
      backbone_trans <-
        vapply(strsplit(as.character(strsplit(backbone[strand_nr],
                                              "")[[1]]), ">"),
               function(x) {
                 paste(indx_backbone[x], collapse = ">")
               }, FUN.VALUE = 'character')
      # add '' manually as the last symbol
      backbone_trans[length(backbone_trans) + 1] <- ""
      
      parsed_xna_obj <-
        paste(sugar_trans, base_trans, backbone_trans, sep = "")
      parsed_xna_obj <- paste0(parsed_xna_obj, collapse = ".")
      paste("RNA", strand_nr, "{", parsed_xna_obj, "}", sep = "")
    }, FUN.VALUE = 'character')
    
    # if any of conjugate slots not missing, extend helms
    if (!is.na(conjugate5) & conjugate5 != "") {
      helms[[1]] <- paste0("CHEM1{", conjugate5, "}|", helms[[1]])
    }
    if (!is.na(conjugate3) & conjugate3 != "") {
      helms[[1]] <- paste0(helms[[1]], "|CHEM1{", conjugate3, "}")
    }
    
    # if base is a 2-elements vector, add pairing information with
    # siRNA_HELM function
    if (length(base) == 2) {
      helms <-
        paste(paste0(helms, collapse = '|'),
              "$",
              siRNA_HELM(xnastring_obj),
              "$$$$V2.0",
              sep = '')
    } else {
      helms <- paste(paste0(helms, collapse = '|'), "$$$$V2.0", sep = '')
    }
    return(helms)
  }


#' siRNA_HELM function takes XNAString object and returns pairing information
#' for base slot. Works only for double stranded molecules.
#' @param xnastring_obj XNAString object
#' @return string
#'
#' @export
#' @examples
#' obj1 <- XNAString(
#'   base = c("CCCCUGCCGUGGUUCAUAA", "UUAUGAACCACGGCAGGGGCG"),
#'   sugar = c("OOFOFOFOFOFOFOFOFOF", "FFOFOFOFOFOFOFOFOFOFO"),
#'   backbone = c("OOOOOOOOOOOOOOOOOO", "OOOOOOOOOOOOOOOOOOOO"),
#'   conjugate3 = c("")
#' )
#'
#' siRNA_HELM(obj1)
siRNA_HELM <- function(xnastring_obj) {
  base <- as.character(base(xnastring_obj))
  
  length(base) == 2 ||
    stop("Pairing infrmormation can be only",
         "returned for double stranded molecules")
  
  base_sense <- changeBase(compl_dictionary(xnastring_obj), base[1])
  base_antisense <-
    changeBase(compl_dictionary(xnastring_obj), base[2])
  
  mat <-
    Biostrings::nucleotideSubstitutionMatrix(match = 1,
                                             mismatch = 0,
                                             baseOnly = TRUE)
  
  pa <- Biostrings::pairwiseAlignment(
    base_sense,
    Biostrings::reverseComplement(Biostrings::DNAStringSet(base_antisense)),
    gapOpening = 1000,
    type = "global-local",
    gapExtension = -1,
    substitutionMatrix = mat
  )
  R1S <- Biostrings::start(Biostrings::pattern(pa))
  R1E <- Biostrings::end(Biostrings::pattern(pa))
  R2S <- Biostrings::start(Biostrings::subject(pa))
  R2E <- Biostrings::end(Biostrings::subject(pa))
  
  n1 <- nchar(base_sense)
  n2 <- nchar(base_antisense)
  p1 <- R1S:R1E
  p2 <- (n2 - R2S + 1):(n2 - R2E + 1)
  
  pair_str <-
    paste(paste0("RNA1,RNA2,", p1 * 3 - 1, ":pair-", p2 * 3 - 1, ":pair"),
          collapse = "|")
  
  return(pair_str)
}
