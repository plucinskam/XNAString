
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
#' my_dic <- data.table::data.table(
#'   type = c(
#'     rep("base", 3),
#'     rep("sugar", 2),
#'     rep("backbone", 3)
#'   ),
#'   symbol = c("G", "E", "A", "F", "O", "S", "B", "X")
#' )
#' obj1 <- XNAString(
#'   name = "a",
#'   base = "GGE",
#'   sugar = "FFO",
#'   backbone = "SB",
#'   dictionary = my_dic
#' )
#' obj2 <- XNAString(
#'   name = "b",
#'   base = "GGE",
#'   sugar = "FFO",
#'   dictionary = my_dic
#' )
#' obj3 <- XNAString(
#'   name = "b",
#'   base = c("GGE", "EEE"),
#'   sugar = c("FFO", "OOO"),
#'   dictionary = my_dic
#' )
#' XNAStringSetObj <- XNAStringSet(objects = list(obj1, obj2, obj3))
#' @author Anna Gorska
#'
#' @include utils.R xnaStringClass.R
#' @importFrom data.table data.table as.data.table
#' @import methods
#'
#' @rdname xnastringSetClass
#'
XNAStringSetMethod <- setClass("XNAStringSet",
                               
                               slots = c(objects = "list"),
                               validity = typedListCheck)

#' Create XNAStringSet object
#' @param objects list of XNAString objects
#' @param base string (or character), RNAString, RNAStringSet, DNAString or
#' DNAStringSet. In use only when objects argument is empty.
#' @param sugar string (or character). In use only when objects argument is
#' empty.
#' @param backbone string (or character). In use only when objects argument
#' is empty.
#' @param name string (or character). In use only when objects argument
#' is empty.
#' @param conjugate5 string (or character). In use only when objects argument
#' is empty.
#' @param conjugate3 string (or character). In use only when objects argument
#' is empty.
#' @param target DNAStringSet, DNAString or character. In use only when objects
#' argument is empty.
#' @param col.base character (name of base column). In use only when objects
#' argument is empty.
#' @param col.sugar character (name of sugar column). In use only when objects
#' argument is empty.
#' @param col.backbone character (name of backbone column). In use only when o
#' bjects argument is empty.
#' @param col.target character (name of target column). In use only when objects
#'  argument is empty.
#' @param col.name character (name of target column). In use only when objects
#'  argument is empty.
#' @param col.conjugate3 character (name of target column). In use only when objects
#'  argument is empty.
#' @param col.conjugate5 character (name of target column). In use only when objects
#'  argument is empty.
#' @param col.base character (name of base column). In use only when objects
#' argument is empty.
#' @param default_sugar character - only one letter. Will be replicated
#'                      nchar(base) times. In use only when objects argument is
#'                       empty.
#' @param default_backbone character - only one letter. Will be replicated
#'                         nchar(base)-1 times. In use only when objects
#'                         argument is empty.
#' @param compl_dict data.table with following columns:
#' "base", "target". By default internal XNAString dictionary is used. In use
#' only when objects argument is empty.
#'
#' @rdname xnastringSetClass
#' @export
# XNAStringSet <- function(objects){
#   return(XNAStringSetMethod(objects = objects))}
XNAStringSet <- function(objects = NA,
                         base = NA,
                         sugar = NA,
                         backbone = NA,
                         target = NA,
                         name = NA,
                         conjugate3 = NA,
                         conjugate5 = NA,
                         col.base = 'base',
                         col.sugar = 'sugar',
                         col.backbone = 'backbone',
                         col.target = 'target',
                         col.conjugate3 = "conjugate3",
                         col.conjugate5 = "conjugate5",
                         col.name = "name",
                         default_sugar = NA,
                         default_backbone = NA,
                         compl_dict = complementary_bases) {
  if (!all(is.na(objects))) {
    set <- XNAStringSetMethod(objects = objects)
  } else if (!is.na(base[1])) {
    dt <- data.table::data.table(
      base = base,
      sugar = sugar,
      backbone = backbone,
      target = target,
      name = name,
      conjugate3 = conjugate3,
      conjugate5 = conjugate5
    )
    cols_not_NA <- colSums(is.na(dt)) < nrow(dt)
    dt <- dt[, cols_not_NA, with = FALSE]
    set <-
      XNAString::dt2Set(
        dt,
        col.base = col.base,
        col.sugar = col.sugar,
        col.backbone = col.backbone,
        col.target = col.target,
        col.name = col.name,
        col.conjugate3 = col.conjugate3,
        col.conjugate5 = col.conjugate5,
        default_sugar,
        default_backbone,
        compl_dict
      )
  } else {
    set <- XNAStringSetMethod(objects = list())
  }
  
  return(set)
}

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
  
  a <- lapply(slots, function(x) {
    list(eval(parse(text = paste(
      "as.character(obj","@", x, ")",
      sep = ''
    ))))
  })
  
  names(a) <- slots
  
  dt <- data.table::setDT(a)
  
  # target is DNAStringSet object and has to be overwritten manually
  if ('target' %in% slots) {
    dt[, 'target'] <- paste(target(obj), sep = '')
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
#'                                  symbol = c('G', 'E', 'A', 'F',
#'                                             'O', 'S', 'B', 'X'))
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
#' @examples
#' my_dic <- data.table::data.table(
#'   type = c(
#'     rep("base", 3),
#'     rep("sugar", 2),
#'     rep("backbone", 3)
#'   ),
#'   symbol = c("G", "E", "A", "F", "O", "S", "B", "X")
#' )
#' obj2 <- XNAString(
#'   name = "b",
#'   base = "GGE",
#'   sugar = "FFO",
#'   dictionary = my_dic
#' )
#' obj3 <- XNAString(
#'   name = "b",
#'   base = c("GGE", "EEE"),
#'   sugar = c("FFO", "OOO"),
#'   dictionary = my_dic
#' )
#' XNAStringSetObj <- XNAStringSet(objects = list(obj2, obj3))
#' set2Dt(XNAStringSetObj, c("base", "sugar"))
set2Dt <- function(obj,
                   slots = c(
                     "name",
                     "base",
                     "sugar",
                     "backbone",
                     "target",
                     "secondary_structure",
                     "conjugate5",
                     "conjugate3"
                   ))
{
  if (length(obj@objects) > 0){
    a <- lapply(seq(1, length(objects(obj))), function(i) {
      xnaObj2Dt(objects(obj)[i][[1]], slots)
    })
    dt <- data.table::rbindlist(a)
  } else {
    dt <- data.table::data.table()
  }
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
#' @param col.name character (name of target column). In use only when objects
#'  argument is empty.
#' @param col.conjugate3 character (name of target column). In use only when objects
#'  argument is empty.
#' @param col.conjugate5 character (name of target column). In use only when objects
#'  argument is empty.
#' @param compl_dict data.table with following columns:
#' "base", "target". By default internal XNAString dictionary is used
#' @param default_sugar character - only one letter. Will be replicated
#'                      nchar(base) times
#' @param default_backbone character - only one letter. Will be replicated
#'                         nchar(base)-1 times
#'
#' @return XNAStringSet object
#'
#' @examples
#' dt <- data.table::data.table(
#'   base = c("TT", "GG"),
#'   sugar = c("FF", "FO"),
#'   backbone = c("S", "S")
#' )
#' dt2Set(dt)
#' @importFrom future.apply future_sapply
#'
#' @export
dt2Set <- function(table,
                   col.base = 'base',
                   col.sugar = 'sugar',
                   col.backbone = 'backbone',
                   col.target = 'target',
                   col.conjugate3 = "conjugate3",
                   col.conjugate5 = "conjugate5",
                   col.name = "name",
                   default_sugar = NA,
                   default_backbone = NA,
                   compl_dict = complementary_bases) {
  stopifnot(class(table) %in% c("data.table", "data.frame"))
  
  c(col.base) %in% colnames(table)  ||
    stop(
      "Table should include at least base column, sugar, ",
      "backbone and target columns are optional"
    )
  
  eval(parse(text = paste("base_vec <- table$", col.base, sep = '')))
  
  if (any(colnames(table) == col.sugar)) {
    eval(parse(text = paste("sugar_vec <- table$", col.sugar, sep = '')))
  }
  if (any(colnames(table) == col.backbone)) {
    eval(parse(text = paste(
      "backbone_vec <- table$", col.backbone, sep = ''
    )))
  }
  if (any(colnames(table) == col.target)) {
    eval(parse(text = paste(
      "target_vec <- table$", col.target, sep = ''
    )))
  }
  
  if (any(colnames(table) == col.name)) {
    eval(parse(text = paste(
      "name_vec <- table$", col.name, sep = ''
    )))
  } else {
    name_vec <- rep(list(NA), nrow(table))
  }
  
  if (any(colnames(table) == col.conjugate3)) {
    eval(parse(text = paste(
      "conjugate3_vec <- table$", col.conjugate3, sep = ''
    )))
  } else {
    conjugate3_vec <- rep(list(NA), nrow(table))
  }
  
  if (any(colnames(table) == col.conjugate5)) {
    eval(parse(text = paste(
      "conjugate5_vec <- table$", col.conjugate5, sep = ''
    )))
  } else {
    conjugate5_vec <- rep(list(NA), nrow(table))
  }
  
  obj_ls <- list()
  if (exists("base_vec") &
      !exists("sugar_vec") &
      !exists("backbone_vec") & !exists("target_vec")) {
    #create list of xnastring objects and check validity
    obj_ls <-
      future.apply::future_sapply(seq(1, nrow(table)), function(i) {
        obj <- eval(parse(
          text = paste(
            "XNAString(base = base_vec[[i]],",
            ifelse(
              !is.na(default_sugar),
              paste("default_sugar = '", default_sugar, "',", sep = ''),
              ""
            ),
            ifelse(
              !is.na(default_backbone),
              paste("default_backbone = '", default_backbone, "',", sep = ''),
              ""
            ),
            "compl_dictionary = compl_dict)",
            sep = ''
          )
        ))
        
        if(!is.na(name_vec[[i]])){
          obj@name <- name_vec[[i]]
        }
        
        if(!is.na(conjugate3_vec[[i]])){
          obj@conjugate3 <- conjugate3_vec[[i]]
        }
        
        if(!is.na(conjugate5_vec[[i]])){
          obj@conjugate5 <- conjugate5_vec[[i]]
        } 
        
        return(obj)
      }, future.globals = structure(FALSE, add = "XNAString"))
  } else if (exists("base_vec") &
             exists("sugar_vec") &
             !exists("backbone_vec") & !exists("target_vec")) {
    #create list of xnastring objects and check validity
    obj_ls <-
      future.apply::future_sapply(seq(1, nrow(table)), function(i) {
        obj <- eval(parse(
          text = paste(
            "XNAString(base = base_vec[[i]],
            sugar = sugar_vec[[i]],
            compl_dictionary = compl_dict)"
          )
        ))
        
        if(!is.na(name_vec[[i]])){
          obj@name <- name_vec[[i]]
        }
        
        if(!is.na(conjugate3_vec[[i]])){
          obj@conjugate3 <- conjugate3_vec[[i]]
        }
        
        if(!is.na(conjugate5_vec[[i]])){
          obj@conjugate5 <- conjugate5_vec[[i]]
        } 
        
        return(obj)
      }, future.globals = structure(FALSE, add = "XNAString"))
  } else if (exists("base_vec") &
             exists("sugar_vec") &
             exists("backbone_vec") & !exists("target_vec")) {
    #create list of xnastring objects and check validity
    obj_ls <-
      future.apply::future_sapply(seq(1, nrow(table)), function(i) {
        obj <- eval(parse(
          text = paste(
            "XNAString(base = base_vec[[i]],
                      sugar = sugar_vec[[i]],
                      backbone = backbone_vec[[i]],
                      compl_dictionary = compl_dict)"
          )
        ))
        
        if(!is.na(name_vec[[i]])){
          obj@name <- name_vec[[i]]
        }
        
        if(!is.na(conjugate3_vec[[i]])){
          obj@conjugate3 <- conjugate3_vec[[i]]
        }
        
        if(!is.na(conjugate5_vec[[i]])){
          obj@conjugate5 <- conjugate5_vec[[i]]
        } 
        
        return(obj)
      }, future.globals = structure(FALSE, add = "XNAString"))
  } else if (exists("base_vec") &
             !exists("sugar_vec") &
             !exists("backbone_vec") & exists("target_vec")) {
    #create list of xnastring objects and check validity
    obj_ls <-
      future.apply::future_sapply(seq(1, nrow(table)), function(i) {
        obj <- eval(parse(
          text = paste(
            "XNAString(base = Biostrings::DNAStringSet(base_vec[[i]]),
                        target = target_vec[[i]],
                        compl_dictionary = compl_dict)"
          )
        ))
        
        if(!is.na(name_vec[[i]])){
          obj@name <- name_vec[[i]]
        }
        
        if(!is.na(conjugate3_vec[[i]])){
          obj@conjugate3 <- conjugate3_vec[[i]]
        }
        
        if(!is.na(conjugate5_vec[[i]])){
          obj@conjugate5 <- conjugate5_vec[[i]]
        } 
        
        return(obj)
      }, future.globals = structure(FALSE, add = "XNAString"))
  } else if (exists("base_vec") &
             exists("sugar_vec") &
             !exists("backbone_vec") & exists("target_vec")) {
    #create list of xnastring objects and check validity
    obj_ls <-
      future.apply::future_sapply(seq(1, nrow(table)), function(i) {
        obj <- eval(parse(
          text = paste(
            "XNAString(base = base_vec[[i]],
                      sugar = sugar_vec[[i]],
                      target = target_vec[[i]],
                      compl_dictionary = compl_dict)"
          )
        ))
        
        if(!is.na(name_vec[[i]])){
          obj@name <- name_vec[[i]]
        }
        
        if(!is.na(conjugate3_vec[[i]])){
          obj@conjugate3 <- conjugate3_vec[[i]]
        }
        
        if(!is.na(conjugate5_vec[[i]])){
          obj@conjugate5 <- conjugate5_vec[[i]]
        } 
        
        return(obj)
      }, future.globals = structure(FALSE, add = "XNAString"))
  } else if (exists("base_vec") &
             exists("sugar_vec") &
             exists("backbone_vec") & exists("target_vec")) {
    #create list of xnastring objects and check validity
    obj_ls <-
      future.apply::future_sapply(seq(1, nrow(table)), function(i) {
        obj <- eval(parse(
          text = paste(
            "XNAString(base = base_vec[[i]],
                      sugar = sugar_vec[[i]],
                      backbone = backbone_vec[[i]],
                      target = target_vec[[i]],
                      compl_dictionary = compl_dict)"
          )
        ))
        if(!is.na(name_vec[[i]])){
          obj@name <- name_vec[[i]]
        }
        
        if(!is.na(conjugate3_vec[[i]])){
          obj@conjugate3 <- conjugate3_vec[[i]]
        }
        
        if(!is.na(conjugate5_vec[[i]])){
          obj@conjugate5 <- conjugate5_vec[[i]]
        } 
        
        return(obj)
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
          function(object) {
            cat("XNAStringSet object\n", sep = "")
            if(length(object) > 0){
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
            } else {
              
            }

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
    stopifnot(class(i) %in% c("numeric", "integer", "character") &
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
      # check if all i's are present in name slot
      stopifnot(all(i %in% dt$name))
      # find rows index for specified names
      i <- dt[name %in% i, which = TRUE]
    }
    return(XNAStringSet(objects = objects(x)[i]))
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
      class(i) %in% c("numeric", "integer", "character", "logical") &
        length(i) == 1 &
        class(x)[[1]] == "XNAStringSet"
    )
    
    # if i of character type, find rows index
    if (is.character(i)) {
      dt <-
        set2Dt(
          x,
          c(
            "name",
            "base",
            "sugar",
            "backbone",
            "target",
            "secondary_structure",
            "conjugate5",
            "conjugate3"
          )
        )
      
      #check if all i's are present in name slot
      stopifnot(all(i %in% dt$name))
      # find rows index for specified names
      i <- dt[name %in% i, which = TRUE]
    }
    
    object <- x@objects[i][[1]]
    return(
      XNAString(
        name = name(object),
        base = base(object),
        sugar = sugar(object),
        backbone = backbone(object),
        target = target(object),
        secondary_structure = secondary_structure(object),
        conjugate5 = conjugate5(object),
        conjugate3 = conjugate3(object),
        dictionary = dictionary(object),
        compl_dictionary = compl_dictionary(object)
      )
    )
  }
)

#' Method to add/replace a single row (either by row index or by 'name' slot)
#' XNAStringSet object is returned.
#' @rdname xnastringSetClass
#' 
#' @export
setMethod(
  f = "[[<-",
  signature = "XNAStringSet",
  definition = function(x, i, value) {
    # extract rows either by rows index or by name slot
    stopifnot(
      class(i) %in% c("numeric", "integer", "character", "logical") &
        length(i) == 1 &
        class(x)[[1]] == "XNAStringSet"
    )
    
    if(i > length(x@objects) + 1){
      stop("Index must be <= ", length(x@objects) + 1)
    }
    
    x@objects[[i]] <- value
    
    return(x)
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
           function(obj) {
             standardGeneric("set2List")
           })

#' @rdname set2List
#' @examples
#' my_dic <- data.table::data.table(type = c(rep('base',3),
#'                                           rep('sugar',2),
#'                                           rep('backbone',3)),
#'                                 symbol = c('G', 'E', 'A', 'F',
#'                                            'O', 'S', 'B', 'X'))
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
          function(obj) {
            ls <- list()
            obj_len <- length(objects(obj))
            
            ls <- lapply(seq(1, obj_len), function(i)
              obj[i])
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