#' Name setter/getter method
#'
#' Getter methods enable extraction of single slots from XNAString
#' and XNAStringSet objects.
#' E.g. name method extracts name slot from XNAString/XNAStringSet
#' object.
#'
#' Setter methods enable overwriting single slots from XNAString
#' and XNAStringSet objects.
#' E.g. name<- method overwrites existing name slot
#'
#' @param x XNAString/XNAStringSet object
#' @param i numeric - possibilities: 1 or 2. If 1 - 1st slots elements printed
#' out, 2nd otherwise. In case the second element is not in the object, empty
#' char created. This parameter is only available for XNAStringSet objects.
#' @param value character vector applied only for setter method
#' @param ... optional arguments to generic function to support
#'  additional methods
#'
#' @return vector in getter method, XNAStringSet object
#'  (with replaced name slot) in setter method
#'
#' @export
#' @rdname name
#' @name name
setGeneric("name", function(x, ...) {
  standardGeneric("name")
})

#' @export
#' @rdname name
#' @examples
#'
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
#' name(obj)
setMethod("name", "XNAString", function(x) {
  as.character(x@name)
})

#' @export
#' @rdname name
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
#'   name = "b",
#'   base = "GGE",
#'   sugar = "FFO",
#'   dictionary = my_dic
#' )
#' obj2 <- XNAString(
#'   name = "b",
#'   base = c("GGE", "EEE"),
#'   sugar = c("FFO", "OOO"),
#'   dictionary = my_dic
#' )
#' XNAStringSetObj <- XNAStringSet(objects = list(obj1, obj2))
#' name(XNAStringSetObj)
setMethod("name", "XNAStringSet", function(x, i = 1) {
  stopifnot(i %in% c(1, 2))
  
  ls <- lapply(x@objects, function(y) {
    as.character(y@name)
  })
  # if i =2 and there are rows with single slot, put '' as a default value
  if (i == 2) {
    len <- unlist(lapply(ls, length))
    if (any(len == 1)) {
      indices <- which(len != i)
      ls[indices] <-
        lapply(ls[indices], function(l) {
          replace(l, 2, "")
        })
    }
  }
  # extract i'th element of vector in ls list
  ls <- lapply(ls, `[[`, i)
  
  unlist(ls)
})


#' @export
#' @rdname name
#'
setGeneric("name<-", function(x, ..., value) {
  standardGeneric("name<-")
})

#' @export
#' @rdname name
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
#' name(obj) <- "new_name"
setMethod("name<-", "XNAString", function(x, value) {
  x@name <- value
  validObject(x)
  x
})


#' @export
#' @rdname name
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
#'   name = "b",
#'   base = "GGE",
#'   sugar = "FFO",
#'   dictionary = my_dic
#' )
#' obj2 <- XNAString(
#'   name = "b",
#'   base = c("GGE", "EEE"),
#'   sugar = c("FFO", "OOO"),
#'   dictionary = my_dic
#' )
#' XNAStringSetObj <- XNAStringSet(objects = list(obj1, obj2))
#' name(XNAStringSetObj, 1) <- c("new1", "new2")
setMethod("name<-", "XNAStringSet", function(x, i = 1, value) {
  stopifnot(i %in% c(1, 2))
  
  x <- x@objects
  
  for (j in seq_len(length(x))) {
    x[[j]]@name <- as.character(x[[j]]@name)
    x[[j]]@name[i] <- value[j]
    validObject(x[[j]])
  }
  
  y <- XNAStringSet(objects = x)
  validObject(y)
  
  return(y)
})


#' Base setter/getter method
#'
#' Getter methods enable extraction of single slots from XNAString
#' and XNAStringSet objects.
#' E.g. name method extracts name slot from XNAString/XNAStringSet
#' object.
#'
#' Setter methods enable overwriting single slots from XNAString
#' and XNAStringSet objects.
#' E.g. name<- method overwrites existing name slot
#'
#' @param x XNAString/XNAStringSet object
#' @param i numeric - possibilities: 1 or 2. If 1 - 1st slots elements printed
#' out, 2nd otherwise. In case the second element is not in the object, empty
#' char created. This parameter is only available for XNAStringSet objects.
#' @param value character vector applied only for setter method
#' @param ... optional arguments to generic function to support
#'  additional methods
#'
#' @return vector in getter method, XNAStringSet object
#'  (with replaced name slot) in setter method
#' @examples
#'
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
#' base(obj)
#' @export
#' @rdname base
#' @name base
setGeneric("base", function(x, ...) {
  standardGeneric("base")
})

#' @export
#' @rdname base
setMethod("base", "XNAString", function(x) {
  as.character(x@base)
})

#' @export
#' @rdname base
setMethod("base", "XNAStringSet", function(x, i = 1) {
  stopifnot(i %in% c(1, 2))
  
  ls <- lapply(x@objects, function(y) {
    as.character(y@base)
  })
  # if i =2 and there are rows with single slot, put '' as a default value
  if (i == 2) {
    len <- unlist(lapply(ls, length))
    if (any(len == 1)) {
      indices <- which(len != i)
      ls[indices] <-
        lapply(ls[indices], function(l) {
          replace(l, 2, "")
        })
    }
  }
  # extract i'th element of vector in ls list
  ls <- lapply(ls, `[[`, i)
  
  unlist(ls)
})

#' @export
#' @rdname base
setGeneric("base<-", function(x, ..., value) {
  standardGeneric("base<-")
})


#' @export
#' @rdname base
setMethod("base<-", "XNAString", function(x, value) {
  x@base <- value
  validObject(x)
  x
})

#' @export
#' @rdname base
setMethod("base<-", "XNAStringSet", function(x, i = 1, value) {
  stopifnot(i %in% c(1, 2))
  
  x <- x@objects
  
  for (j in seq_len(length(x))) {
    x[[j]]@base <- as.character(x[[j]]@base)
    x[[j]]@base[i] <- value[j]
    validObject(x[[j]])
  }
  
  y <- XNAStringSet(objects = x)
  validObject(y)
  
  return(y)
})



#' Sugar setter/getter method
#'
#' Getter methods enable extraction of single slots from XNAString
#' and XNAStringSet objects.
#' E.g. name method extracts name slot from XNAString/XNAStringSet
#' object.
#'
#' Setter methods enable overwriting single slots from XNAString
#' and XNAStringSet objects.
#' E.g. name<- method overwrites existing name slot
#'
#' @param x XNAString/XNAStringSet object
#' @param i numeric - possibilities: 1 or 2. If 1 - 1st slots elements printed
#' out, 2nd otherwise. In case the second element is not in the object, empty
#' char created. This parameter is only available for XNAStringSet objects.
#' @param value character vector applied only for setter method
#' @param ... optional arguments to generic function to support
#'  additional methods
#'
#' @return vector in getter method, XNAStringSet object
#'  (with replaced name slot) in setter method
#' @examples
#'
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
#' sugar(obj)
#' @export
#' @rdname sugar
#' @name sugar
setGeneric("sugar", function(x, ...) {
  standardGeneric("sugar")
})

#' @export
#' @rdname sugar
setMethod("sugar", "XNAString", function(x) {
  as.character(x@sugar)
})

#' @export
#' @rdname sugar
setMethod("sugar", "XNAStringSet", function(x, i = 1) {
  stopifnot(i %in% c(1, 2))
  
  ls <- lapply(x@objects, function(y) {
    as.character(y@sugar)
  })
  # if i =2 and there are rows with single slot, put '' as a default value
  if (i == 2) {
    len <- unlist(lapply(ls, length))
    if (any(len == 1)) {
      indices <- which(len != i)
      ls[indices] <-
        lapply(ls[indices], function(l) {
          replace(l, 2, "")
        })
    }
  }
  # extract i'th element of vector in ls list
  ls <- lapply(ls, `[[`, i)
  
  unlist(ls)
})


#' @export
#' @rdname sugar
setGeneric("sugar<-", function(x, ..., value) {
  standardGeneric("sugar<-")
})


#' @export
#' @rdname sugar
setMethod("sugar<-", "XNAString", function(x, value) {
  x@sugar <- value
  validObject(x)
  x
})

#' @export
#' @rdname sugar
setMethod("sugar<-", "XNAStringSet", function(x, i = 1, value) {
  stopifnot(i %in% c(1, 2))
  
  x <- x@objects
  
  for (j in seq_len(length(x))) {
    x[[j]]@sugar <- as.character(x[[j]]@sugar)
    x[[j]]@sugar[i] <- value[j]
    validObject(x[[j]])
  }
  
  y <- XNAStringSet(objects = x)
  validObject(y)
  
  return(y)
})








#' Backbone setter/getter method
#'
#' Getter methods enable extraction of single slots from XNAString
#' and XNAStringSet objects.
#' E.g. name method extracts name slot from XNAString/XNAStringSet
#' object.
#'
#' Setter methods enable overwriting single slots from XNAString
#' and XNAStringSet objects.
#' E.g. name<- method overwrites existing name slot
#'
#' @param x XNAString/XNAStringSet object
#' @param i numeric - possibilities: 1 or 2. If 1 - 1st slots elements printed
#' out, 2nd otherwise. In case the second element is not in the object, empty
#' char created. This parameter is only available for XNAStringSet objects.
#' @param value character vector applied only for setter method
#' @param ... optional arguments to generic function to support
#'  additional methods
#'
#' @return vector in getter method, XNAStringSet object
#'  (with replaced name slot) in setter method
#' @examples
#'
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
#' backbone(obj)
#' @export
#' @rdname backbone
#' @name backbone
setGeneric("backbone", function(x, ...) {
  standardGeneric("backbone")
})

#' @export
#' @rdname backbone
setMethod("backbone", "XNAString", function(x) {
  as.character(x@backbone)
})

#' @export
#' @rdname backbone
setMethod("backbone", "XNAStringSet", function(x, i = 1) {
  stopifnot(i %in% c(1, 2))
  
  ls <- lapply(x@objects, function(y) {
    as.character(y@backbone)
  })
  # if i =2 and there are rows with single slot, put '' as a default value
  if (i == 2) {
    len <- unlist(lapply(ls, length))
    if (any(len == 1)) {
      indices <- which(len != i)
      ls[indices] <-
        lapply(ls[indices], function(l) {
          replace(l, 2, "")
        })
    }
  }
  # extract i'th element of vector in ls list
  ls <- lapply(ls, `[[`, i)
  
  unlist(ls)
})

#' @export
#' @rdname backbone
setGeneric("backbone<-", function(x, ..., value) {
  standardGeneric("backbone<-")
})

#' @export
#' @rdname backbone
setMethod("backbone<-", "XNAString", function(x, value) {
  x@backbone <- value
  validObject(x)
  x
})

#' @export
#' @rdname backbone
setMethod("backbone<-", "XNAStringSet", function(x, i = 1, value) {
  stopifnot(i %in% c(1, 2))
  
  x <- x@objects
  
  for (j in seq_len(length(x))) {
    x[[j]]@backbone <- as.character(x[[j]]@backbone)
    x[[j]]@backbone[i] <- value[j]
    validObject(x[[j]])
  }
  
  y <- XNAStringSet(objects = x)
  validObject(y)
  
  return(y)
})








#' Target setter/getter method
#'
#' Getter methods enable extraction of single slots from XNAString
#' and XNAStringSet objects.
#' E.g. name method extracts name slot from XNAString/XNAStringSet
#' object.
#'
#' Setter methods enable overwriting single slots from XNAString
#' and XNAStringSet objects.
#' E.g. name<- method overwrites existing name slot
#'
#' @param x XNAString/XNAStringSet object
#' @param i numeric - possibilities: 1 or 2. If 1 - 1st slots elements printed
#' out, 2nd otherwise. In case the second element is not in the object, empty
#' char created. This parameter is only available for XNAStringSet objects.
#' @param value character vector applied only for setter method
#' @param ... optional arguments to generic function to support
#'  additional methods
#'
#' @return vector in getter method, XNAStringSet object
#'  (with replaced name slot) in setter method
#' @examples
#'
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
#' target(obj)
#' @export
#' @rdname target
#' @name target
setGeneric("target", function(x, ...) {
  standardGeneric("target")
})

#' @export
#' @rdname target
setMethod("target", "XNAString", function(x) {
  as.character(x@target)
})

#' @export
#' @rdname target
setMethod("target", "XNAStringSet", function(x, i = 1) {
  stopifnot(i %in% c(1, 2))
  
  ls <- lapply(x@objects, function(y) {
    as.character(y@target)
  })
  # if i =2 and there are rows with single slot, put '' as a default value
  if (i == 2) {
    len <- unlist(lapply(ls, length))
    if (any(len == 1)) {
      indices <- which(len != i)
      ls[indices] <-
        lapply(ls[indices], function(l) {
          replace(l, 2, "")
        })
    }
  }
  # extract i'th element of vector in ls list
  ls <- lapply(ls, `[[`, i)
  
  unlist(ls)
})


#' @export
#' @rdname target
setGeneric("target<-", function(x, ..., value) {
  standardGeneric("target<-")
})

#' @export
#' @rdname target
setMethod("target<-", "XNAString", function(x, value) {
  x@target <- value
  validObject(x)
  x
})

#' @export
#' @rdname target
setMethod("target<-", "XNAStringSet", function(x, i = 1, value) {
  stopifnot(i %in% c(1, 2))
  
  x <- x@objects
  
  for (j in seq_len(length(x))) {
    x[[j]]@target <- as.character(x[[j]]@target)
    x[[j]]@target[i] <- value[j]
    validObject(x[[j]])
  }
  
  y <- XNAStringSet(objects = x)
  validObject(y)
  
  return(y)
})





#' Conjugate5 setter/getter method
#'
#' Getter methods enable extraction of single slots from XNAString
#' and XNAStringSet objects.
#' E.g. name method extracts name slot from XNAString/XNAStringSet
#' object.
#'
#' Setter methods enable overwriting single slots from XNAString
#' and XNAStringSet objects.
#' E.g. name<- method overwrites existing name slot
#'
#' @param x XNAString/XNAStringSet object
#' @param i numeric - possibilities: 1 or 2. If 1 - 1st slots elements printed
#' out, 2nd otherwise. In case the second element is not in the object, empty
#' char created. This parameter is only available for XNAStringSet objects.
#' @param value character vector applied only for setter method
#' @param ... optional arguments to generic function to support
#'  additional methods
#'
#' @return vector in getter method, XNAStringSet object
#'  (with replaced name slot) in setter method
#' @examples
#'
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
#' conjugate5(obj)
#' @export
#' @rdname conjugate5
#' @name conjugate5
setGeneric("conjugate5", function(x, ...) {
  standardGeneric("conjugate5")
})

#' @export
#' @rdname conjugate5
setMethod("conjugate5", "XNAString", function(x) {
  as.character(x@conjugate5)
})

#' @export
#' @rdname conjugate5
setMethod("conjugate5", "XNAStringSet", function(x, i = 1) {
  stopifnot(i %in% c(1, 2))
  
  ls <- lapply(x@objects, function(y) {
    as.character(y@conjugate5)
  })
  # if i =2 and there are rows with single slot, put '' as a default value
  if (i == 2) {
    len <- unlist(lapply(ls, length))
    if (any(len == 1)) {
      indices <- which(len != i)
      ls[indices] <-
        lapply(ls[indices], function(l) {
          replace(l, 2, "")
        })
    }
  }
  # extract i'th element of vector in ls list
  ls <- lapply(ls, `[[`, i)
  
  unlist(ls)
})



#' @export
#' @rdname conjugate5
setGeneric("conjugate5<-", function(x, ..., value) {
  standardGeneric("conjugate5<-")
})

#' @export
#' @rdname conjugate5
setMethod("conjugate5<-", "XNAString", function(x, value) {
  x@conjugate5 <- value
  validObject(x)
  x
})

#' @export
#' @rdname conjugate5
setMethod("conjugate5<-", "XNAStringSet", function(x, i = 1, value) {
  stopifnot(i %in% c(1, 2))
  
  x <- x@objects
  
  for (j in seq_len(length(x))) {
    x[[j]]@conjugate5 <- as.character(x[[j]]@conjugate5)
    x[[j]]@conjugate5[i] <- value[j]
    validObject(x[[j]])
  }
  
  y <- XNAStringSet(objects = x)
  validObject(y)
  
  return(y)
})







#' Conjugate3 setter/getter method
#'
#' Getter methods enable extraction of single slots from XNAString
#' and XNAStringSet objects.
#' E.g. name method extracts name slot from XNAString/XNAStringSet
#' object.
#'
#' Setter methods enable overwriting single slots from XNAString
#' and XNAStringSet objects.
#' E.g. name<- method overwrites existing name slot
#'
#' @param x XNAString/XNAStringSet object
#' @param i numeric - possibilities: 1 or 2. If 1 - 1st slots elements printed
#' out, 2nd otherwise. In case the second element is not in the object, empty
#' char created. This parameter is only available for XNAStringSet objects.
#' @param value character vector applied only for setter method
#' @param ... optional arguments to generic function to
#'  support additional methods
#'
#' @return vector in getter method, XNAStringSet object
#'  (with replaced name slot) in setter method
#' @examples
#'
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
#' conjugate3(obj)
#' @export
#' @rdname conjugate3
#' @name conjugate3
setGeneric("conjugate3", function(x, ...) {
  standardGeneric("conjugate3")
})

#' @export
#' @rdname conjugate3
setMethod("conjugate3", "XNAString", function(x) {
  as.character(x@conjugate3)
})

#' @export
#' @rdname conjugate3
setMethod("conjugate3", "XNAStringSet", function(x, i = 1) {
  stopifnot(i %in% c(1, 2))
  
  ls <- lapply(x@objects, function(y) {
    as.character(y@conjugate3)
  })
  # if i =2 and there are rows with single slot, put '' as a default value
  if (i == 2) {
    len <- unlist(lapply(ls, length))
    if (any(len == 1)) {
      indices <- which(len != i)
      ls[indices] <-
        lapply(ls[indices], function(l) {
          replace(l, 2, "")
        })
    }
  }
  # extract i'th element of vector in ls list
  ls <- lapply(ls, `[[`, i)
  
  unlist(ls)
})


#' @export
#' @rdname conjugate3
setGeneric("conjugate3<-", function(x, ..., value) {
  standardGeneric("conjugate3<-")
})

#' @export
#' @rdname conjugate3
setMethod("conjugate3<-", "XNAString", function(x, value) {
  x@conjugate3 <- value
  validObject(x)
  x
})

#' @export
#' @rdname conjugate3
setMethod("conjugate3<-", "XNAStringSet", function(x, i = 1, value) {
  stopifnot(i %in% c(1, 2))
  
  x <- x@objects
  
  for (j in seq_len(length(x))) {
    x[[j]]@conjugate3 <- as.character(x[[j]]@conjugate3)
    x[[j]]@conjugate3[i] <- value[j]
    validObject(x[[j]])
  }
  
  y <- XNAStringSet(objects = x)
  validObject(y)
  
  return(y)
})









#' Dictionary setter/getter method
#'
#' Getter methods enable extraction of single slots from XNAString
#' and XNAStringSet objects.
#' E.g. name method extracts name slot from XNAString/XNAStringSet
#' object.
#'
#' Setter methods enable overwriting single slots from XNAString
#' and XNAStringSet objects.
#' E.g. name<- method overwrites existing name slot
#'
#' @param x XNAString/XNAStringSet object
#' @param value character vector applied only for setter method
#' @param ... optional arguments to generic function to support
#'  additional methods
#'
#' @return vector in getter method, XNAStringSet object
#'  (with replaced name slot) in setter method
#' @examples
#'
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
#' dictionary(obj)
#' @export
#' @rdname dictionary
#' @name dictionary
setGeneric("dictionary", function(x, ...) {
  standardGeneric("dictionary")
})

#' @export
#' @rdname dictionary
setMethod("dictionary", "XNAString", function(x) {
  x@dictionary
})


#' @export
#' @rdname dictionary
setGeneric("dictionary<-", function(x, ..., value) {
  standardGeneric("dictionary<-")
})

#' @export
#' @rdname dictionary
setMethod("dictionary<-", "XNAString", function(x, value) {
  x@dictionary <- value
  validObject(x)
  x
})





#' Secondary_structure setter/getter method
#'
#' Getter methods enable extraction of single slots from XNAString
#' and XNAStringSet objects.
#' E.g. name method extracts name slot from XNAString/XNAStringSet
#' object.
#'
#' Setter methods enable overwriting single slots from XNAString
#' and XNAStringSet objects.
#' E.g. name<- method overwrites existing name slot
#'
#' @param x XNAString/XNAStringSet object
#' @param value character vector applied only for setter method
#' @param ... optional arguments to generic function to support
#'  additional methods
#'
#' @return vector in getter method, XNAStringSet object
#'  (with replaced name slot) in setter method
#' @examples
#'
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
#' secondary_structure(obj)
#' @export
#' @rdname secondary_structure
#' @name secondary_structure
setGeneric("secondary_structure", function(x, ...) {
  standardGeneric("secondary_structure")
})

#' @export
#' @rdname secondary_structure
setMethod("secondary_structure", "XNAString", function(x) {
  x@secondary_structure
})

#' @export
#' @rdname secondary_structure
setMethod("secondary_structure", "XNAStringSet", function(x) {
  ls <- lapply(x@objects, function(y) {
    paste(y@secondary_structure, collapse = ", ")
  })
  
  unlist(ls)
})

#' @export
#' @rdname secondary_structure
setGeneric("secondary_structure<-", function(x, ..., value) {
  standardGeneric("secondary_structure<-")
})

#' @export
#' @rdname secondary_structure
setMethod("secondary_structure<-", "XNAString", function(x, value) {
  x@secondary_structure <- value
  validObject(x)
  x
})






#' Compl_dictionary setter/getter method
#'
#' Getter methods enable extraction of single slots from XNAString
#' and XNAStringSet objects.
#' E.g. name method extracts name slot from XNAString/XNAStringSet
#' object.
#'
#' Setter methods enable overwriting single slots from XNAString
#' and XNAStringSet objects.
#' E.g. name<- method overwrites existing name slot
#'
#' @param x XNAString/XNAStringSet object
#' @param value character vector applied only for setter method
#' @param ... optional arguments to generic function to support
#'  additional methods
#'
#' @return vector in getter method, XNAStringSet object
#'  (with replaced name slot) in setter method
#' @examples
#'
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
#' compl_dictionary(obj)
#' @export
#' @rdname compl_dictionary
#' @name compl_dictionary
setGeneric("compl_dictionary", function(x, ...) {
  standardGeneric("compl_dictionary")
})

#' @export
#' @rdname compl_dictionary
setMethod("compl_dictionary", "XNAString", function(x) {
  x@compl_dictionary
})


#' @export
#' @rdname compl_dictionary
setGeneric("compl_dictionary<-", function(x, ..., value) {
  standardGeneric("compl_dictionary<-")
})

#' @export
#' @rdname compl_dictionary
setMethod("compl_dictionary<-", "XNAString", function(x, value) {
  x@compl_dictionary <- value
  validObject(x)
  x
})






#' Duplex_structure setter/getter method
#'
#' Getter methods enable extraction of single slots from XNAString
#' and XNAStringSet objects.
#' E.g. name method extracts name slot from XNAString/XNAStringSet
#' object.
#'
#' Setter methods enable overwriting single slots from XNAString
#' and XNAStringSet objects.
#' E.g. name<- method overwrites existing name slot
#'
#' @param x XNAString/XNAStringSet object
#' @param value character vector applied only for setter method
#' @param ... optional arguments to generic function to support
#'  additional methods
#'
#' @return vector in getter method, XNAStringSet object
#'  (with replaced name slot) in setter method
#' @examples
#'
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
#' duplex_structure(obj)
#' @export
#' @rdname duplex_structure
#' @name duplex_structure
setGeneric("duplex_structure", function(x, ...) {
  standardGeneric("duplex_structure")
})

#' @export
#' @rdname duplex_structure
setMethod("duplex_structure", "XNAString", function(x) {
  x@duplex_structure
})

#' @export
#' @rdname duplex_structure
setMethod("duplex_structure", "XNAStringSet", function(x) {
  ls <- lapply(x@objects, function(y) {
    paste(y@duplex_structure, collapse = ", ")
  })
  
  unlist(ls)
})


#' @export
#' @rdname duplex_structure
setGeneric("duplex_structure<-", function(x, ..., value) {
  standardGeneric("duplex_structure<-")
})

#' @export
#' @rdname duplex_structure
setMethod("duplex_structure<-", "XNAString", function(x, value) {
  x@duplex_structure <- value
  validObject(x)
  x
})