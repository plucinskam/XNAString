#' Translate RNA from HELM notation to multi-string notation
#'
#' This function translates RNA molecules encoded in HELM notation
#'   into multi-string notation. It uses dictionary which links HELM
#'   code for base, sugar and backbone elements with
#'   symbols used in multi-string notation.
#'
#' @param helm string with HELM sequence, which contains one RNA polymer and
#'   optionally CHEM element
#' @param dictionary data.table with following columns:
#' "HELM", "type", "symbol".
#'  By default internal XNAString dictionary is used.
#' @param remove_linker logical defines if linker should be clipped from RNA
#' @return named list of strings with following elements: base, sugar, backbone,
#'   conjugate5, conjugate3
#' @examples helm2String("RNA1{[dR](A)P.[dR](A)P.[dR](A)}$$$$V2.0")
#' @importFrom data.table is.data.table
#' @importFrom stringi stri_length stri_sub
#'
#' @author Marianna Plucinska
#' @export
#'
helm2String <-
  function(helm,
           dictionary = xna_dictionary,
           remove_linker = TRUE) {
    data.table::is.data.table(dictionary) ||
      stop("dictionary should be a data.table.")

    all(c("HELM", "type", "symbol") %in% names(dictionary)) ||
      stop("dictionary should be a data.table with 'HELM', '
         type' and 'symbol' column")

    helm_stripped <- strsplit(x = helm, split = "\\$")[[1]][1]
    helm_components <-
      strsplit(x = helm_stripped, split = "\\|")[[1]]

    any(startsWith(helm_components, "RNA")) ||
      stop("There is no RNA component in HELM notation.")

    is_conjugate <- startsWith(helm_components, "CHEM")
    conjugate5 <- ""
    conjugate3 <- ""

    if (any(is_conjugate)) {
      if (is_conjugate[1]) {
        conjugate5 <-
          gsub(pattern = list("CHEM[1-9]\\{|\\}"), "", helm_components[1])
      }

      if (is_conjugate[-1]) {
        conjugate3 <-
          gsub(pattern = list("CHEM[1-9]\\{|\\}"), "", helm_components[-1])
      }
    }

    rna_component <-
      helm_components[startsWith(helm_components, "RNA")]

    !(any(is_conjugate) & length(rna_component) != 1) ||
      stop(
        "Current implementation of helm2String does not support translation
        of HELM sequneces with mulitiple RNA polimer and CHEM"
      )

    helm_rna <-
      gsub(pattern = list("RNA[1-9]\\{|\\}"), "", rna_component)

    helm_rna_monomers <- strsplit(helm_rna, split = "[.]")

    rna_multistring <-
      lapply(helm_rna_monomers, function(rna_monomer) {
        parseRnaHelmComponent(rna_monomer, dictionary = dictionary)
      })

    rna_multistring <- listOflists2Dt(rna_multistring)
    base <- rna_multistring$base
    sugar <- rna_multistring$sugar
    backbone <- rna_multistring$backbone

    if (remove_linker) {
      if (conjugate5 != "") {
        while (stringi::stri_sub(backbone, 1, 1) == "O" &
          stringi::stri_sub(sugar, 1, 1) != "L") {
          sugar <- stringi::stri_sub(sugar, 2)
          backbone <- stringi::stri_sub(backbone, 2)
          base <- stringi::stri_sub(base, 2)
        }
      }

      if (conjugate3 != "") {
        while (stringi::stri_sub(backbone, stringi::stri_length(backbone)) == "O" &
          stringi::stri_sub(sugar, stringi::stri_length(sugar)) != "L") {
          sugar <-
            stringi::stri_sub(sugar, 1, stringi::stri_length(sugar) - 1)
          backbone <-
            stringi::stri_sub(backbone, 1, stringi::stri_length(backbone) - 1)
          base <-
            stringi::stri_sub(base, 1, stringi::stri_length(base) - 1)
        }
      }
    }

    return(
      list(
        base = base,
        sugar = sugar,
        backbone = backbone,
        conjugate3 = conjugate3,
        conjugate5 = conjugate5
      )
    )
  }


#' Parse monomers from HELM to multi-string notation
#'
#' @param rna_component list of monomers building RNA
#' @param dictionary data.table with following columns:
#' "HELM", "type", "symbol".
#'   By default internal XNAString dictionary is used.
#' @return list of three strings: base, sugar, backbone
#'
#' @importFrom stringi stri_extract
#' @importFrom data.table as.data.table
#'
#' @examples parseRnaHelmComponent(c("[dR](A)P", "[dR](A)P", "[dR](A)"))
#' @author Marianna Plucinska
#' @export
parseRnaHelmComponent <-
  function(rna_component,
           dictionary = xna_dictionary) {
    rna_multistring <-
      lapply(rna_component, function(monomer) {
        base_seq <- stringi::stri_extract(monomer, regex = "\\((.*)\\)")
        sugar_seq <-
          stringi::stri_extract(monomer, regex = "(.*)(?=\\()")
        backbone_seq <-
          stringi::stri_extract(monomer, regex = "(?<=\\))(.*)")
        if (!any(base_seq == dictionary[dictionary$type == "base"][["HELM"]]) &
          !is.na(base_seq)) {
          stop(sprintf("Base %s is not present in dictionary.", base_seq))
        }

        if (!any(sugar_seq == dictionary[dictionary$type == "sugar"][["HELM"]]) &
          !is.na(sugar_seq)) {
          stop(sprintf("Sugar %s is not present in dictionary.", sugar_seq))
        }

        if (!any(backbone_seq == dictionary[dictionary$type == "backbone"][["HELM"]]) &
          !is.na(backbone_seq) & backbone_seq != "") {
          stop(sprintf("Backbone %s is not present in dictionary.", backbone_seq))
        }

        base_symbol <-
          dictionary[dictionary$type == "base" &
            dictionary$HELM == base_seq][["symbol"]]
        sugar_symbol <-
          dictionary[dictionary$type == "sugar" &
            dictionary$HELM == sugar_seq][["symbol"]]
        backbone_symbol <-
          dictionary[dictionary$type == "backbone" &
            dictionary$HELM == backbone_seq][["symbol"]]

        return(list(
          base = base_symbol,
          sugar = sugar_symbol,
          backbone = backbone_symbol
        ))
      })

    rna_multistring <-
      data.table::as.data.table(do.call(rbind, rna_multistring))

    base <- paste(unlist(rna_multistring[["base"]]), collapse = "")
    sugar <-
      paste(unlist(rna_multistring[["sugar"]]), collapse = "")
    backbone <-
      paste(unlist(rna_multistring[["backbone"]]), collapse = "")

    return(list(
      base = base,
      sugar = sugar,
      backbone = backbone
    ))
  }
