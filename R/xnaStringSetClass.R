
#' Create class which consists of XNAString objects given as a list
#' @param objects list of XNAString objects
#' @param object XNAStringSet object
#' @param x XNAStringSet object
#' @param i numeric, integer, character, logical - filter needed for extraction 
#' method
#'
#' @return XNASTringSet object
#'
#' @examples
#' my_dic <- data.table::data.table(type = c(rep('base',3),
#'                                           rep('sugar',2),
#'                                           rep('backbone',3)),
#'                                symbol = c('G', 'E', 'A', 'F', 'O', 'S', 'B', 'X'))
#' obj1 <- XNAString(name = 'a',
#'                   base = 'GGE',
#'                   sugar = 'FFO',
#'                   backbone ='SB',
#'                   dictionary = my_dic)
#' obj2 <- XNAString(name = 'b',
#'                   base = 'GGE',
#'                   sugar = 'FFO',
#'                   dictionary = my_dic)
#' obj3 <- XNAString(name = 'b',
#'                   base = c('GGE','EEE'),
#'                   sugar = c('FFO', 'OOO'),
#'                   dictionary = my_dic)
#' XNAStringSetObj <- XNAStringSet(objects=list(obj1, obj2, obj3))
#'
#' @author Anna Gorska
#' 
#' @include utils.R
#' @importFrom data.table data.table as.data.table
#' @import methods
#'
#' @rdname xnastringSetClass
#'
XNAStringSetMethod <- setClass("XNAStringSet",
                         
                         slots = c(objects = "list"),
                         validity = typedListCheck)


#' @rdname xnastringSetClass
#' @export
XNAStringSet <- function(objects){
  return(XNAStringSetMethod(objects = objects))}



#'
#' xnaObj2Dt function - changes XNAString object to data.table
#'
#' @param obj XNAString object
#' @param slots slots that are saved as column names (possibilities: "name", 
#'             "base", "sugar", "backbone", "target", "conjugate5", 
#'             "conjugate3" and  "dictionary" )
#'
#' @return data.table
#'
#' @importFrom data.table setDT
#'
xnaObj2Dt <- function(obj, slots) {

  dt <- data.table::data.table()
  dt <- data.table::setDT(as.list(sapply(slots, function(x)
    eval(
      parse(
        text = paste("dt[ ,'", x, "'] <- list(list(obj@", x, "))" ,
                     sep = '')
      )
    ))))
  # target is DNAStringSet object and has to be overwritten manually
  if ('target' %in% slots){
    dt[, 'target'] <- paste(obj@target, sep = '')
  }
  
  return(dt)
}


#'
#' set2Dt function - changes XNAStringSet object to data.table
#'
#' @param obj XNAStringSet object
#' @param slots slots that are saved as column names (possibilities: "name", 
#'             "base", "sugar", "backbone", "target", "conjugate5", 
#'             "conjugate3" and  "dictionary" )
#'
#' @return data.table
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
#' set2Dt(XNAStringSetObj, c('base', 'sugar'))
#'
set2Dt <- function(obj, slots) {

  M <- sapply(seq(1, length(obj@objects)), function(i){
       xnaObj2Dt(obj@objects[i][[1]], slots)
  })
  # if there is just slot, list returned instead of matrix
  if(length(slots) == 1){
    M <- matrix(M)
    colnames(M) <- slots
    M <- t(M)
  }
  df <- as.data.frame(t(M))
  # df columns are lists of lists
  dt <- as.data.table(apply(df, 2, unlist, recursive = FALSE))
  
  return(dt)
}


#' Function which creates XNAstringSet object from table with
#' base, sugar and backbone columns.
#' @param table data.table or data.frame (must incluse 
#'              base, sugar and backbone columns)
#' @param col.base character (name of base column)
#' @param col.sugar character (name of sugar column)
#' @param col.backbone character (name of backbone column)
#' @param col.target character (name of target column)
#'
#' @return XNAStringSet object
#'
#' @examples
#' dt <- data.table::data.table(base= c('TT', 'GG'), 
#'                  sugar = c('FF', 'FO'), 
#'                  backbone =c('S', 'S'))
#' dt2Set(dt)
#' 
#' @importFrom future.apply future_sapply
#'
#' @export
dt2Set <- function(table,
                   col.base = 'base',
                   col.sugar = 'sugar',
                   col.backbone = 'backbone',
                   col.target = 'target') {
  
  stopifnot(class(table) %in% c("data.table", "data.frame"))
  
  all(c(col.base, col.sugar, col.backbone) %in% colnames(table))  ||
    stop("Table shoud include base, sugar and backbone columns, target is optional")
  
  obj_ls <- list()
  if(!col.target %in% colnames(table)){
    #create list of xnastring objects and check validity
    obj_ls <- future.apply::future_sapply(seq(1, nrow(table)), function(i) {
      eval(parse(
        text = paste(
          "XNAString(base = unlist(table$",
          col.base,
          "[i]), sugar = unlist(table$",
          col.sugar,
          "[i]), backbone = unlist(table$",
          col.backbone,
          "[i]))",
          sep = ''
        )
      ))
    }, future.globals = structure(FALSE, add = "XNAString"))
  } else {
    obj_ls <- future.apply::future_sapply(seq(1, nrow(table)), function(i) {
      eval(parse(
        text = paste(
          "XNAString(base = unlist(table$",
          col.base,
          "[i]), sugar = unlist(table$",
          col.sugar,
          "[i]), backbone = unlist(table$",
          col.backbone,
          "[i]), target = unlist(table$",
          col.target,
          "[i]))",
          sep = ''
        )
      ))
    }, future.globals = structure(FALSE, add = "XNAString"))
  }
  # create XNAStringSet object
  obj_set <- XNAStringSet(objects = obj_ls)
  
  return(obj_set)
}

#'
#' Define show method
#' @rdname xnastringSetClass
#' @export
setMethod("show", "XNAStringSet",
          function(object)
          {
            cat("XNAStringSet object\n", sep = "")
            res <-
              set2Dt(
                object,
                c(
                  "name",
                  "base",
                  "sugar",
                  "backbone",
                  "target",
                  "conjugate5",
                  "conjugate3",
                  "secondary_structure"
                )
              )
            print(res)
          })



#' Method to extract a row/rows (either by row index or by 'name' slot)
#' XNAStringSet object is returned.
#' @rdname xnastringSetClass
#' @aliases extractionMethods
#' @export
setMethod(
  f = "[",
  signature = "XNAStringSet",
  definition = function(x, i) {
    # extract rows either by rows index or by name slot
    stopifnot(class(i) %in% c('numeric', 'integer', 'character') &
                class(x)[[1]] == "XNAStringSet")
    
    # if i of character type, find rows index
    if (is.character(i)) {
      dt <-
        set2Dt(x,
               c(
                 "name",
                 "base",
                 "sugar",
                 "backbone",
                 "target",
                 "conjugate5",
                 "conjugate3"
               ))
      #check if all i's are present in name slot
      stopifnot(all(i %in% dt$name))
      # find rows index for specified names
      i <- dt[name %in% i, which = TRUE]
    }
    
    return(XNAStringSet(objects = x@objects[i]))
    
  }
)


#'
#' Method to extract a single row (either by row index or by 'name' slot)
#' XNAString object is returned.
#' @rdname xnastringSetClass
#' @export
setMethod(
  f = "[[",
  signature = "XNAStringSet",
  definition = function(x, i) {
    # extract rows either by rows index or by name slot
    stopifnot(
      class(i) %in% c('numeric', 'integer', 'character', 'logical') &
        length(i) == 1 &
        class(x)[[1]] == "XNAStringSet"
    )
    
    # if i of character type, find rows index
    if (is.character(i)) {
      dt <-
        set2Dt(x,
               c(
                 "name",
                 "base",
                 "sugar",
                 "backbone",
                 "target",
                 "secondary_structure",
                 "conjugate5",
                 "conjugate3"
               ))
      #check if all i's are present in name slot
      stopifnot(all(i %in% dt$name))
      # find rows index for specified names
      i <- dt[name %in% i, which = TRUE]
    }
    
    object <- x@objects[i][[1]]
    return(
      XNAString(
        name = object@name,
        base = object@base,
        sugar = object@sugar,
        backbone = object@backbone,
        target = object@target,
        secondary_structure = object@secondary_structure,
        conjugate5 = object@conjugate5,
        conjugate3 = object@conjugate3,
        dictionary = object@dictionary,
        compl_dictionary = object@compl_dictionary
      )
    )
  }
)



#'
#' Define method to save XNAStringSet object as a list of XNAString objects
#'
#' @param obj XNAStringSet object
#'
#' @return list of XNAString objects
#'
#' @rdname set2List
#' @export
setGeneric("set2List",
           function(obj)
             standardGeneric("set2List"))

#' @rdname set2List
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
#' set2List(XNAStringSetObj)
setMethod("set2List", "XNAStringSet",
          function(obj){
            ls <- list()
            obj_len <- length(obj@objects)
              
            ls <- sapply(seq(1, obj_len), function(i) obj[i])
            return(ls)
          })


#' XNAString2XNAStringSet function - changes XNAString object to XNAStringSet
#'
#' @param XNAString_obj XNAString object
#'
#' @return XNAStringSet object
#'
XNAString2XNAStringSet <- function(XNAString_obj) {
  class(XNAString_obj)[[1]] == "XNAString" ||
    stop("An object must be of XNAString class")
  
  XNAStringSet_obj <- XNAStringSet(objects = list(XNAString_obj))
  
  return(XNAStringSet_obj)
}
