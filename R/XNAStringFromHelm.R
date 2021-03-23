#' Create XNAString object from HELM - user interface
#'
#' @param helm string (or strings vector) with HELM sequence, which contains
#' one RNA polymer and optionally CHEM element
#' @param name character (or character vector)
#' @param dictionary data.table with following columns:
#' "HELM", "type", "symbol".
#' By default internal XNAString dictionary is used.
#' @param remove_linker logical defines if linker should be clipped from RNA
#' @param compl_dictionary data.table with following columns:
#'   "base", "target". By default internal XNAString dictionary is used
#' @return XNAString object if single helm, XNAStringSet object otherwise
#' @examples
#' XNAStringFromHelm("RNA1{[dR](A)P.[dR](A)P.[dR](A)}$$$$V2.0")
#' XNAStringFromHelm("RNA1{[dR](A)P.[dR](A)P.[dR](A)}$$$$V2.0", 'name')
#' XNAStringFromHelm(c("RNA1{[dR](A)P.[dR](A)P.[dR](A)}$$$$V2.0",
#'                     "RNA1{[dR](T)P.[dR](T)P.[dR](A)}$$$$V2.0"), 
#'                   c('name1', 'name2'))
#'                   
#' @importFrom future.apply future_sapply
#' 
#' @export
#' @author Marianna Plucinska
#' 
XNAStringFromHelm <-
  function(helm,
           name = NA_character_,
           dictionary = xna_dictionary,
           compl_dictionary = complementary_bases,
           remove_linker = TRUE) {
  
    obj_ls <- future.apply::future_sapply(seq(1,length(helm)), function(i) {
      multistring <-
        helm2String(helm = helm[i],
                    dictionary = dictionary,
                    remove_linker = remove_linker)
      
      XNAString(
        name = name[i],
        base = multistring$base,
        sugar = multistring$sugar,
        backbone = multistring$backbone,
        conjugate3 = multistring$conjugate3,
        conjugate5 = multistring$conjugate5,
        dictionary = dictionary,
        compl_dictionary = complementary_bases
      )
      
    })
    
    # if multiple helm, create XNAStringSet, XNAString object otherwise
    if (length(obj_ls) > 1) {
      XNAStringSet(objects = obj_ls)
    } else {
      obj_ls[[1]]
    }
  }