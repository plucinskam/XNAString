#' Set of getter methods
#' 
#' Getter methods enable extraction of single slots from XNAString
#' and XNAStringSet objects.
#' E.g. name method extracts name slot from XNAString/XNAStringSet 
#' object.
#'
#' @param x XNAString/XNAStringSet object
#' @param i numeric - possibilities: 1 or 2. If 1 - 1st slots elements printed 
#' out, 2nd otherwise. In case the second element is not in the object, empty 
#' char created. This parameter is only available for XNAStringSet objects.
#' @param ... optional arguments to generic function to support additional methods    
#'
#' @return vector
#'
#' @export
#' @docType methods
#' @rdname getterMethods
#' 
#' @aliases getterMethods name
setGeneric("name",  function(x, ...)
  standardGeneric("name"))

#' @export
#' @rdname getterMethods
#' @aliases getterMethods name XNAString
#' @examples 
#' 
#' my_dic <- data.table::data.table(type = c(rep('base',3),
#'                                           rep('sugar',2),
#'                                           rep('backbone',3)),
#'                                symbol = c('G', 'E', 'A', 'F', 'O', 'S', 'B', 'X'))
#' obj <- XNAString(name = 'b',
#'                   base = 'GGE',
#'                   sugar = 'FFO',
#'                   dictionary = my_dic)
#' name(obj)
setMethod("name", "XNAString", function(x)
  as.character(x@name))

#' @export
#' @rdname getterMethods
#' @aliases getterMethods name XNAStringSet
#' @examples 
#' my_dic <- data.table::data.table(type = c(rep('base',3),
#'                                           rep('sugar',2),
#'                                           rep('backbone',3)),
#'                                symbol = c('G', 'E', 'A', 'F', 'O', 'S', 'B', 'X'))
#' obj1 <- XNAString(name = 'b',
#'                   base = 'GGE',
#'                   sugar = 'FFO',
#'                   dictionary = my_dic)
#' obj2 <- XNAString(name = 'b',
#'                   base = c('GGE','EEE'),
#'                   sugar = c('FFO', 'OOO'),
#'                   dictionary = my_dic)
#' XNAStringSetObj <- XNAStringSet(objects=list(obj1, obj2))
#' name(XNAStringSetObj)
setMethod("name", "XNAStringSet", function(x, i = 1) {
  stopifnot(i %in% c(1, 2))
  
  ls <- lapply(x@objects, function(y)
    as.character(y@name))
  # if i =2 and there are rows with single slot, put '' as a default value
  if (i == 2) {
    len <- unlist(lapply(ls, length))
    if (any(len == 1)) {
      indices <- which(len != i)
      ls[indices] <-
        lapply(ls[indices], function(l)
          replace(l, 2, ''))
    }
  }
  # extract i'th element of vector in ls list
  ls <- lapply(ls, `[[`, i)
  
  unlist(ls)
})




#' @export
#' @rdname getterMethods
#' @aliases getterMethods base
setGeneric("base",  function(x, ...)
  standardGeneric("base"))

#' @export
#' @rdname getterMethods
#' @aliases getterMethods base XNAString
setMethod("base", "XNAString", function(x)
  as.character(x@base))

#' @export
#' @rdname getterMethods
#' @aliases getterMethods base XNAStringSet
setMethod("base", "XNAStringSet", function(x, i = 1) {
  stopifnot(i %in% c(1, 2))
  
  ls <- lapply(x@objects, function(y)
    as.character(y@base))
  # if i =2 and there are rows with single slot, put '' as a default value
  if (i == 2) {
    len <- unlist(lapply(ls, length))
    if (any(len == 1)) {
      indices <- which(len != i)
      ls[indices] <-
        lapply(ls[indices], function(l)
          replace(l, 2, ''))
    }
  }
  # extract i'th element of vector in ls list
  ls <- lapply(ls, `[[`, i)
  
  unlist(ls)
})

#' @export
#' @rdname getterMethods
#' @aliases getterMethods sugar
setGeneric("sugar",  function(x, ...)
  standardGeneric("sugar"))

#' @export
#' @rdname getterMethods
#' @aliases getterMethods sugar XNAString
setMethod("sugar", "XNAString", function(x)
  as.character(x@sugar))

#' @export
#' @rdname getterMethods
#' @aliases getterMethods sugar XNAStringSet
setMethod("sugar", "XNAStringSet", function(x, i = 1) {
  stopifnot(i %in% c(1, 2))
  
  ls <- lapply(x@objects, function(y)
    as.character(y@sugar))
  # if i =2 and there are rows with single slot, put '' as a default value
  if (i == 2) {
    len <- unlist(lapply(ls, length))
    if (any(len == 1)) {
      indices <- which(len != i)
      ls[indices] <-
        lapply(ls[indices], function(l)
          replace(l, 2, ''))
    }
  }
  # extract i'th element of vector in ls list
  ls <- lapply(ls, `[[`, i)
  
  unlist(ls)
})



#' @export
#' @rdname getterMethods
#' @aliases getterMethods backbone
setGeneric("backbone",  function(x, ...)
  standardGeneric("backbone"))

#' @export
#' @rdname getterMethods
#' @aliases getterMethods backbone XNAString
setMethod("backbone", "XNAString", function(x)
  as.character(x@backbone))

#' @export
#' @rdname getterMethods
#' @aliases getterMethods backbone XNAStringSet
setMethod("backbone", "XNAStringSet", function(x, i = 1) {
  stopifnot(i %in% c(1, 2))
  
  ls <- lapply(x@objects, function(y)
    as.character(y@backbone))
  # if i =2 and there are rows with single slot, put '' as a default value
  if (i == 2) {
    len <- unlist(lapply(ls, length))
    if (any(len == 1)) {
      indices <- which(len != i)
      ls[indices] <-
        lapply(ls[indices], function(l)
          replace(l, 2, ''))
    }
  }
  # extract i'th element of vector in ls list
  ls <- lapply(ls, `[[`, i)
  
  unlist(ls)
})


#' @export
#' @rdname getterMethods
#' @aliases getterMethods target
setGeneric("target",  function(x, ...)
  standardGeneric("target"))

#' @export
#' @rdname getterMethods
#' @aliases getterMethods target XNAString
setMethod("target", "XNAString", function(x)
  as.character(x@target))

#' @export
#' @rdname getterMethods
#' @aliases getterMethods target XNAStringSet
setMethod("target", "XNAStringSet", function(x, i = 1) {
  stopifnot(i %in% c(1, 2))
  
  ls <- lapply(x@objects, function(y)
    as.character(y@target))
  # if i =2 and there are rows with single slot, put '' as a default value
  if (i == 2) {
    len <- unlist(lapply(ls, length))
    if (any(len == 1)) {
      indices <- which(len != i)
      ls[indices] <-
        lapply(ls[indices], function(l)
          replace(l, 2, ''))
    }
  }
  # extract i'th element of vector in ls list
  ls <- lapply(ls, `[[`, i)
  
  unlist(ls)
})




#' @export
#' @rdname getterMethods
#' @aliases getterMethods conjugate5
setGeneric("conjugate5",  function(x, ...)
  standardGeneric("conjugate5"))

#' @export
#' @rdname getterMethods
#' @aliases getterMethods conjugate5 XNAString
setMethod("conjugate5", "XNAString", function(x)
  as.character(x@conjugate5))

#' @export
#' @rdname getterMethods
#' @aliases getterMethods conjugate5 XNAStringSet
setMethod("conjugate5", "XNAStringSet", function(x, i = 1) {
  stopifnot(i %in% c(1, 2))
  
  ls <- lapply(x@objects, function(y)
    as.character(y@conjugate5))
  # if i =2 and there are rows with single slot, put '' as a default value
  if (i == 2) {
    len <- unlist(lapply(ls, length))
    if (any(len == 1)) {
      indices <- which(len != i)
      ls[indices] <-
        lapply(ls[indices], function(l)
          replace(l, 2, ''))
    }
  }
  # extract i'th element of vector in ls list
  ls <- lapply(ls, `[[`, i)
  
  unlist(ls)
})



#' @export
#' @rdname getterMethods
#' @aliases getterMethods conjugate3
setGeneric("conjugate3",  function(x, ...)
  standardGeneric("conjugate3"))

#' @export
#' @rdname getterMethods
#' @aliases getterMethods conjugate3 XNAString
setMethod("conjugate3", "XNAString", function(x)
  as.character(x@conjugate3))

#' @export
#' @rdname getterMethods
#' @aliases getterMethods conjugate3 XNAStringSet
setMethod("conjugate3", "XNAStringSet", function(x, i = 1) {
  stopifnot(i %in% c(1, 2))
  
  ls <- lapply(x@objects, function(y)
    as.character(y@conjugate3))
  # if i =2 and there are rows with single slot, put '' as a default value
  if (i == 2) {
    len <- unlist(lapply(ls, length))
    if (any(len == 1)) {
      indices <- which(len != i)
      ls[indices] <-
        lapply(ls[indices], function(l)
          replace(l, 2, ''))
    }
  }
  # extract i'th element of vector in ls list
  ls <- lapply(ls, `[[`, i)
  
  unlist(ls)
})


#' @export
#' @rdname getterMethods
#' @aliases getterMethods dictionary
setGeneric("dictionary",  function(x, ...)
  standardGeneric("dictionary"))

#' @export
#' @rdname getterMethods
#' @aliases getterMethods dictionary XNAString
setMethod("dictionary", "XNAString", function(x)
  x@dictionary)


#' @export
#' @rdname getterMethods
#' @aliases getterMethods secondary_structure
setGeneric("secondary_structure",  function(x, ...)
  standardGeneric("secondary_structure"))

#' @export
#' @rdname getterMethods
#' @aliases getterMethods secondary_structure XNAString
setMethod("secondary_structure", "XNAString", function(x)
  x@secondary_structure)


#' @export
#' @rdname getterMethods
#' @aliases getterMethods secondary_structure XNAStringSet
setMethod("secondary_structure", "XNAStringSet", function(x) {
  ls <- lapply(x@objects, function(y)
    paste(y@secondary_structure, collapse = ', '))
  
  unlist(ls)
})

          
#' @export
#' @rdname getterMethods
#' @aliases getterMethods compl_dictionary
setGeneric("compl_dictionary",  function(x, ...)
  standardGeneric("compl_dictionary"))

#' @export
#' @rdname getterMethods
#' @aliases getterMethods compl_dictionary XNAString
setMethod("compl_dictionary", "XNAString", function(x)
  x@compl_dictionary)


#' @export
#' @rdname getterMethods
#' @aliases getterMethods duplex_structure
setGeneric("duplex_structure",  function(x, ...)
  standardGeneric("duplex_structure"))

#' @export
#' @rdname getterMethods
#' @aliases getterMethods duplex_structure XNAString
setMethod("duplex_structure", "XNAString", function(x)
  x@duplex_structure)

#' @export
#' @rdname getterMethods
#' @aliases getterMethods duplex_structure XNAStringSet
setMethod("duplex_structure", "XNAStringSet", function(x) {
  ls <- lapply(x@objects, function(y)
    paste(y@duplex_structure, collapse = ', '))
  
  unlist(ls)
})




#' Set of setter methods
#' 
#' Setter methods enable overwriting single slots from XNAString
#' and XNAStringSet objects. 
#' E.g. name<- method overwrites existing name slot

#'
#' @param x XNAString/XNAStringSet object
#' @param value string vector
#' @param i numeric - possibilities: 1 or 2. If 1 - 1st slots elements printed 
#' out, 2nd otherwise. In case the second element is not in the object, empty 
#' char created. This parameter is only available for XNAStringSet objects.
#' @param ... optional arguments to generic function to support additional methods    
#'
#' @return XNAStringSet object (with replaced name slot)
#'
#' @export
#' @docType methods
#' @rdname setterMethods
#' 
#' @aliases setterMethods name
setGeneric("name<-", function(x, value, ...)
  standardGeneric("name<-"))

#' @export
#' @rdname setterMethods
#' @aliases setterMethods name XNAString
#' @examples 
#' my_dic <- data.table::data.table(type = c(rep('base',3),
#'                                           rep('sugar',2),
#'                                           rep('backbone',3)),
#'                                symbol = c('G', 'E', 'A', 'F', 'O', 'S', 'B', 'X'))
#' obj <- XNAString(name = 'b',
#'                   base = 'GGE',
#'                   sugar = 'FFO',
#'                   dictionary = my_dic)
#' name(obj) <- 'new_name'
setMethod("name<-", "XNAString", function(x, value) {
  x@name <- value
  validObject(x)
  x
})


#' @export
#' @rdname setterMethods
#' @aliases setterMethods name XNAStringSet
#' @examples 
#' my_dic <- data.table::data.table(type = c(rep('base',3),
#'                                           rep('sugar',2),
#'                                           rep('backbone',3)),
#'                                symbol = c('G', 'E', 'A', 'F', 'O', 'S', 'B', 'X'))
#' obj1 <- XNAString(name = 'b',
#'                   base = 'GGE',
#'                   sugar = 'FFO',
#'                   dictionary = my_dic)
#' obj2 <- XNAString(name = 'b',
#'                   base = c('GGE','EEE'),
#'                   sugar = c('FFO', 'OOO'),
#'                   dictionary = my_dic)
#' XNAStringSetObj <- XNAStringSet(objects=list(obj1, obj2))
#' name(XNAStringSetObj) <- c('new1', 'new2')
setMethod("name<-", "XNAStringSet", function(x, value, i = 1) {
  stopifnot(i %in% c(1,2))
  
  x <- x@objects
  
  for(j in 1:length(x)){
    x[[j]]@name <- as.character(x[[j]]@name)
    x[[j]]@name[i] <- value[j]
    validObject(x[[j]])
  }
  
  y <-  XNAStringSet(objects=x)
  validObject(y)
  
  return(y)
})



#' @export
#' @rdname setterMethods
#' @aliases setterMethods base
setGeneric("base<-", function(x, value, ...)
  standardGeneric("base<-"))


#' @export
#' @rdname setterMethods
#' @aliases setterMethods base XNAString
setMethod("base<-", "XNAString", function(x, value) {
  x@base <- value
  validObject(x)
  x
})

#' @export
#' @rdname setterMethods
#' @aliases setterMethods base XNAStringSet
setMethod("base<-", "XNAStringSet", function(x, value, i=1) {
  stopifnot(i %in% c(1,2))
  
  x <- x@objects
  
  for(j in 1:length(x)){
    x[[j]]@base <- as.character(x[[j]]@base)
    x[[j]]@base[i] <- value[j]
    validObject(x[[j]])
  }
  
  y <-  XNAStringSet(objects=x)
  validObject(y)
  
  return(y)
})


#' @export
#' @rdname setterMethods
#' @aliases setterMethods sugar
setGeneric("sugar<-", function(x, value, ...)
  standardGeneric("sugar<-"))

#' @export
#' @rdname setterMethods
#' @aliases setterMethods sugar XNAString
setMethod("sugar<-", "XNAString", function(x, value) {
  x@sugar <- value
  validObject(x)
  x
})

#' @export
#' @rdname setterMethods
#' @aliases setterMethods sugar XNAStringSet
setMethod("sugar<-", "XNAStringSet", function(x, value, i = 1) {
  stopifnot(i %in% c(1,2))
  
  x <- x@objects
  
  for(j in 1:length(x)){
    x[[j]]@sugar <- as.character(x[[j]]@sugar)
    x[[j]]@sugar[i] <- value[j]
    validObject(x[[j]])
  }
  
  y <-  XNAStringSet(objects=x)
  validObject(y)
  
  return(y)
})



#' @export
#' @rdname setterMethods
#' @aliases setterMethods backbone
setGeneric("backbone<-", function(x, value, ...)
  standardGeneric("backbone<-"))

#' @export
#' @rdname setterMethods
#' @aliases setterMethods backbone XNAString
setMethod("backbone<-", "XNAString", function(x, value) {
  x@backbone <- value
  validObject(x)
  x
})

#' @export
#' @rdname setterMethods
#' @aliases setterMethods sugar XNAStringSet
setMethod("backbone<-", "XNAStringSet", function(x, value, i = 1) {
  stopifnot(i %in% c(1,2))
  
  x <- x@objects
  
  for(j in 1:length(x)){
    x[[j]]@backbone <- as.character(x[[j]]@backbone)
    x[[j]]@backbone[i] <- value[j]
    validObject(x[[j]])
  }
  
  y <-  XNAStringSet(objects=x)
  validObject(y)
  
  return(y)
})


#' @export
#' @rdname setterMethods
#' @aliases setterMethods target
setGeneric("target<-", function(x, value, ...)
  standardGeneric("target<-"))

#' @export
#' @rdname setterMethods
#' @aliases setterMethods target XNAString
setMethod("target<-", "XNAString", function(x, value) {
  x@target <- value
  validObject(x)
  x
})

#' @export
#' @rdname setterMethods
#' @aliases setterMethods target XNAStringSet
setMethod("target<-", "XNAStringSet", function(x, value, i = 1) {
  stopifnot(i %in% c(1,2))
  
  x <- x@objects
  
  for(j in 1:length(x)){
    x[[j]]@target <- as.character(x[[j]]@target)
    x[[j]]@target[i] <- value[j]
    validObject(x[[j]])
  }
  
  y <-  XNAStringSet(objects=x)
  validObject(y)
  
  return(y)
})




#' @export
#' @rdname setterMethods
#' @aliases setterMethods conjugate5
setGeneric("conjugate5<-", function(x, value, ...)
  standardGeneric("conjugate5<-"))

#' @export
#' @rdname setterMethods
#' @aliases setterMethods conjugate5 XNAString
setMethod("conjugate5<-", "XNAString", function(x, value) {
  x@conjugate5 <- value
  validObject(x)
  x
})

#' @export
#' @rdname setterMethods
#' @aliases setterMethods conjugate5 XNAStringSet
setMethod("conjugate5<-", "XNAStringSet", function(x, value, i = 1) {
  stopifnot(i %in% c(1,2))
  
  x <- x@objects
  
  for(j in 1:length(x)){
    x[[j]]@conjugate5 <- as.character(x[[j]]@conjugate5)
    x[[j]]@conjugate5[i] <- value[j]
    validObject(x[[j]])
  }
  
  y <-  XNAStringSet(objects=x)
  validObject(y)
  
  return(y)
})



#' @export
#' @rdname setterMethods
#' @aliases setterMethods conjugate3
setGeneric("conjugate3<-", function(x, value, ...)
  standardGeneric("conjugate3<-"))

#' @export
#' @rdname setterMethods
#' @aliases setterMethods conjugate3 XNAString
setMethod("conjugate3<-", "XNAString", function(x, value) {
  x@conjugate3 <- value
  validObject(x)
  x
})

#' @export
#' @rdname setterMethods
#' @aliases setterMethods conjugate3 XNAStringSet
setMethod("conjugate3<-", "XNAStringSet", function(x, value, i = 1) {
  stopifnot(i %in% c(1,2))
  
  x <- x@objects
  
  for(j in 1:length(x)){
    x[[j]]@conjugate3 <- as.character(x[[j]]@conjugate3)
    x[[j]]@conjugate3[i] <- value[j]
    validObject(x[[j]])
  }
  
  y <-  XNAStringSet(objects=x)
  validObject(y)
  
  return(y)
})


#' @export
#' @rdname setterMethods
#' @aliases setterMethods dictionary
setGeneric("dictionary<-", function(x, value, ...)
  standardGeneric("dictionary<-"))

#' @export
#' @rdname setterMethods
#' @aliases setterMethods dictionary XNAString
setMethod("dictionary<-", "XNAString", function(x, value) {
  x@dictionary <- value
  validObject(x)
  x
})

#' @export
#' @rdname setterMethods
#' @aliases setterMethods compl_dictionary
setGeneric("compl_dictionary<-", function(x, value, ...)
  standardGeneric("compl_dictionary<-"))

#' @export
#' @rdname setterMethods
#' @aliases setterMethods compl_dictionary XNAString
setMethod("compl_dictionary<-", "XNAString", function(x, value) {
  x@compl_dictionary <- value
  validObject(x)
  x
})

#' @export
#' @rdname setterMethods
#' @aliases setterMethods secondary_structure
setGeneric("secondary_structure<-", function(x, value, ...)
  standardGeneric("secondary_structure<-"))

#' @export
#' @rdname setterMethods
#' @aliases setterMethods secondary_structure XNAString
setMethod("secondary_structure<-", "XNAString", function(x, value) {
  x@secondary_structure <- value
  validObject(x)
  x
})


#' @export
#' @rdname setterMethods
#' @aliases setterMethods duplex_structure
setGeneric("duplex_structure<-", function(x, value, ...)
  standardGeneric("duplex_structure<-"))

#' @export
#' @rdname setterMethods
#' @aliases setterMethods duplex_structure XNAString
setMethod("duplex_structure<-", "XNAString", function(x, value) {
  x@duplex_structure <- value
  validObject(x)
  x
})