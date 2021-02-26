#'
#' Check on an object type
#'
#' @param object an object of any class
#' @param type class of an object
#'
#' @return logical information. TRUE if \code{object} class equals \code{type}
#'
#' @examples
#' instanceOf(1, 'numeric')
#'
#' @export
instanceOf = function(object, type) {
  class(object)[[1]] == type
}


#'
#' Check if all objects are of XNAString class and dictionaries are the same
#'
#' @param object an object of any class. An object must contain
#' 'objects' (list type) slot
#'
#' @return logical information. Checks the whole list of objects,
#' TRUE if class of all objects equals 'XNAString' and their dictionaries
#' are the same.
#'
#' @export
#'
#' @examples 
#' my_dic <- data.table::data.table(type = c(rep('base',3),
#'                                           rep('sugar',2),
#'                                           rep('backbone',3)),
#'                                symbol = c('G', 'E', 'A', 'F', 'O', 'S', 'B', 'X'))
#' obj2 <- XNAString(name = 'b',
#'                   base = 'GGE',
#'                   sugar = 'FFO',
#'                   dictionary = my_dic)
#' obj3 <- XNAString(name = 'b',
#'                   base = c('GGE','EEE'),
#'                   sugar = c('FFO', 'OOO'),
#'                   dictionary = my_dic)
#' XNAStringSetObj <- XNAStringSet(objects=list(obj2, obj3))
#' typedListCheck(XNAStringSetObj)
#' 
typedListCheck <- function(object) {
  errors <- character()
  is_correct_object_type <- vector()
  equal_dict <- vector()
  
  stopifnot(length(object@objects) >= 1)
  
  for (i in seq_len(length(object@objects))) {
    obj <- object@objects[[i]]
    is_correct_object_type[[i]] <- instanceOf(obj, 'XNAString')
    # check if the 1st dictionary equals to all the rest
    equal_dict[[i]] <-
      all.equal(
        object@objects[[1]]@dictionary,
        obj@dictionary,
        ignore.col.order = TRUE,
        ignore.row.order = TRUE
      )
  }
  
  if (any(!is_correct_object_type)) {
    msg <-
      "At least one object is not of XNAString type."
    errors <- c(errors, msg)
  }
  
  if (any(equal_dict != TRUE)) {
    msg <-
      "Dictionary slot is not equal for all XNAString objects"
    errors <- c(errors, msg)
  }
  
  if (length(errors) == 0)
    TRUE
  else
    errors
}




#' Utility functions useful when programming and developing XNAString class
#'
### =========================================================================
### Unique characters in string vector
### -------------------------------------------------------------------------
#' @param x A string vector
#'
#' @return A list of vectors with unique characters found in \code{x} string
#'
#' @examples
#' uniqueChars('TRGFFTR')
#' uniqueChars(c('TRGFFTR', 'AATGRC'))
#'
#' @export
uniqueChars <- function(x) {
  stopifnot(is.character(x))
  list_of_unique <- lapply(strsplit(x, ""), unique)
  
  return(list_of_unique)
}


#' 
#' Save list of lists as data.table
#' 
#' @param list_of_lists list of lists that will be saved as data.table.
#'
#' @return data.table
#'
#' @examples
#' nested_list <- list(list(base = c('T'), sugar = c('G')), 
#'                     list(base = c('U'), sugar = c('G')))
#' listOflists2Dt(nested_list)
#'
#' @export
#'
listOflists2Dt <- function(list_of_lists) {
  matrix_t <-
    t(matrix(unlist(list_of_lists), nrow = length(unlist(list_of_lists[1]))))
  matrix_2_dt <- data.table::data.table(matrix_t)
  names(matrix_2_dt) <- names(list_of_lists[[1]])
  
  return(matrix_2_dt)
}




#' 
#' Function which checks if XNAString object satisfies predefined slots length
#' 
#' @param xnastring_obj XNAString object
#' @param cond_base allowed base elements in object
#' @param cond_sugar allowed sugar elements in object
#' @param cond_backbone allowed backbone elements in object
#' @param cond_name allowed name elements in object
#' @param cond_target allowed target elements in object
#' @param cond_conj5 allowed conj5 elements in object
#' @param cond_conj3 allowed conj3 elements in object
#'
#' @return logical
#'
#' @examples
#' obj <- XNAString(base = c('EAA', 'AAA'), 
#'                  sugar = c('FFO', 'OOO'), 
#'                  name = c('a'), 
#'                  conjugate5 = c('TTT'))
#' xnastringElementsNumber(obj, 
#'                         cond_name = '==1', 
#'                         cond_base = '%in% c(1,2)', 
#'                         cond_sugar = '%in% c(1,2)', 
#'                         cond_backbone = '%in% c(1,2)',
#'                         cond_target = '>0', 
#'                         cond_conj5 = '==1', 
#'                         cond_conj = '==1')
#'
#' @export
#'
xnastringElementsNumber <-
  function(xnastring_obj,
           cond_name = '==1',
           cond_base,
           cond_sugar,
           cond_backbone,
           cond_target = '>0',
           cond_conj5 = '==1',
           cond_conj3 = '==1') {
    res <- FALSE
    
    if (eval(parse(
      text = paste(
        "length(xnastring_obj@name)",
        cond_name,
        "&& length(xnastring_obj@base)",
        cond_base,
        "&& length(xnastring_obj@sugar)",
        cond_sugar,
        "&& length(xnastring_obj@backbone)",
        cond_backbone,
        "&& length(xnastring_obj@target)",
        cond_target,
        "&& length(xnastring_obj@conjugate5)",
        cond_conj5,
        "&& length(xnastring_obj@conjugate3)",
        cond_conj3,
        sep = " "
      )
    ))) {
      res <- TRUE
    }
    
    return(res)
  }




#' 
#' Concatenate HELM-symbol custom dictionary with built-in HELM-symbol 
#' dictionary (xna_dictionary)
#' 
#' @param custom_dict custom HELM-symbol dictionary
#' @param default_dict built-in HELM-symbol dictionary (xna_dictionary)
#' @param helm_colname helm column name in custom dictionary
#' @param type_colname type column name in custom dictionary
#' @param symbol_colname symbol column name in custom dictionary
#' 
#' @return data.table 
#'
#' @examples
#' my_dict <- data.table::data.table(HELM = c('[[B]]'),
#'                                   type = c('base'),
#'                                   symbol = c('B'))
#' concatDict(my_dict)
#'
#' @export
#'
concatDict <- function(custom_dict,
                        default_dict = xna_dictionary,
                        helm_colname = "HELM",
                        type_colname = "type",
                        symbol_colname = "symbol") {
  all(c(helm_colname, type_colname, symbol_colname) %in% colnames(custom_dict)) ||
    stop("HELM, type and symbol columns are required in custom HELM-symbol dictionary")
  
  dict <- rbind(custom_dict,
                xna_dictionary)
  
  !(any(duplicated(dict[,c('type', 'symbol')]))) || 
    stop("There is at least one duplicated symbol for the same type.")
  
  !(any(duplicated(dict[,c('type', 'HELM')]))) || 
    stop("There is at least one duplicated HELM for the same type.")
  
  return(dict)
}





#' 
#' Translate base slot based on complementary bases dictionary.
#' Base sequence in transformed using compl_target column.
#' 
#' @param compl_dict complementary bases dictionary
#' @param bases string, one or two-elements vector
#' @return string 
#'
changeBase <- function(compl_dict, bases) {
  all(c("base", "target", "compl_target") %in% colnames(compl_dict)) ||
    stop("Complementary bases dictionary must include base, target and compl_target columns")
  
  if (all(unlist(strsplit(bases, "")) %in% compl_dict$base) &
      !any(is.na(compl_dict$compl_target)) &
      !any(compl_dict$compl_target == '')) {
    complement <- sapply(bases, function(base) {
      chartr(
        paste(compl_dict$base, collapse = ''),
        paste(compl_dict$compl_target, collapse = ''),
        base
      )
    })
    
  } else {
    complement <- ''
  }
  
  return(unname(complement))
}



