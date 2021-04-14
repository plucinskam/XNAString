#' Development of XNAString class aims at enabling efficient manipulation of
#' modified oligonucleotide sequences. The class consists of the following
#' slots: name, base, sugar, backbone, target, conjugate5, conjugate3,
#' secondary_structure, duplex_structure, dictionary (HELM-string dictionary),
#' compl_dictionary.
#'
#' The package inherits some of the functionalities from Biostrings package.
#' In contrary to Biostrings sequences, XNAString classes allow for description
#' of base sequence, sugar and backbone in a single object.
#' XNAString is able to capture single stranded oligonucleotides, siRNAs, PNAs,
#' shRNAs, gRNAs and synthetic mRNAs, and enable users to apply
#' sequence-manipulating Bioconductor packages to their analysis.
#' XNAString can read and write a HELM notation, compute alphabet frequency,
#' align and match targets.
#'
#' @param name string (or character)
#' @param base string (or character), RNAString, RNAStringSet, DNAString or
#' DNAStringSet
#' @param sugar string (or character)
#' @param backbone string (or character)
#' @param target DNAStringSet, DNAString or character
#' @param conjugate5 string (or character)
#' @param conjugate3 string (or character)
#' @param secondary_structure list
#' @param duplex_structure list
#' @param dictionary data.table with following columns:
#'   "HELM", "type", "symbol". By default internal XNAString dictionary is used.
#' @param compl_dictionary data.table with following columns:
#'   "base", "target". By default internal XNAString dictionary is used
#' @param default_sugar character, a single letter which will be replicated
#'                     in sugar slot as default value
#' @param default_backbone character, a single letter which will be replicated
#'                         in backbone slot as default value
#' @param object XNAString object
#' @param .Object XNAString object
#' @param x A single string specifying the type of sequences
#'
#' @return Object which consists of \code{name}, \code{base}, \code{sugar},
#' \code{backbone}, \code{target}, \code{conjugate5}, \code{conjugate3},
#'  \code{secondary_structure},  \code{duplex_structure},
#' \code{dictionary}, \code{compl_dictionary}.
#'
#'
#' @examples
#' obj1 <- XNAString(
#'   base = "ATCG",
#'   sugar = "FODD",
#'   conjugate3 = "TAG"
#' )
#' obj2 <- XNAString(
#'   base = "ATCG",
#'   sugar = "FODD",
#'   backbone = "SBB"
#' )
#' str(obj2)
#' name(obj2) <- 'a'
#' base(obj2) <- 'ATTT'
#' sugar(obj2) <- 'LMFF'
#' backbone(obj2) <- 'BAB'
#' conjugate5(obj2) <- 'TFJSJG'
#' conjugate3(obj2) <- 'ARTSS'
#' my_dic <- data.table::data.table(type = c(rep('base',3),
#'                                           rep('sugar',2),
#'                                           rep('backbone',3)),
#'                                 symbol = c('G', 'E', 'A', 'F',
#'                                            'O', 'S', 'B', 'X'))
#' obj1 <- XNAString(base = 'AAE',
#'                   sugar = 'FFO',
#'                   backbone='SB',
#'                   dictionary = my_dic)
#' obj2 <- XNAString(base = c('EAA', 'AAAA'),
#'                   sugar = c('FFO', 'OOOO'),
#'                   name = c('a'),
#'                   conjugate5 = c('TTT'),
#'                   dictionary = my_dic)
#'
#' my_dic <- data.table::data.table(
#'   type = c(
#'     rep("base", 3),
#'     rep("sugar", 2),
#'     rep("backbone", 3)
#'   ),
#'   symbol = c("G", "E", "A", "F", "O", "S", "B", "X")
#' )
#' obj1 <- XNAString(
#'   base = "AAE",
#'   sugar = "FFO",
#'   backbone = "SB",
#'   dictionary = my_dic
#' )
#' obj2 <- XNAString(
#'   base = c("EAA", "AAAA"),
#'   sugar = c("FFO", "OOOO"),
#'   name = c("a"),
#'   conjugate5 = c("TTT"),
#'   dictionary = my_dic
#' )
#' @author Anna Gorska
#'
#' @importFrom data.table data.table
#' @importClassesFrom Biostrings BString DNAString RNAString DNAStringSet
#' RNAStringSet
#' @import methods
#'
#' @importClassesFrom Biostrings BString DNAString RNAString DNAStringSet
#' RNAStringSet
#'
#' @docType methods
#' @rdname xnastringClass
#' @include classUnion.R
#' @include setterGetter.R
### =========================================================================
### XNAString class definition
### -------------------------------------------------------------------------
###
### The XNAString class is the XString heir (subclass)
###
XNAStringMethod <- setClass(
  "XNAString",
  contains = "BString",
  slots = c(
    name = "character",
    base = "charOrDNAOrRNA",
    sugar = "character",
    backbone = "character",
    target = "charOrDNA",
    conjugate5 = "character",
    conjugate3 = "character",
    secondary_structure = "list",
    duplex_structure = "list",
    dictionary = "data.table",
    compl_dictionary = "data.table",
    default_sugar = "character",
    default_backbone = "character"
  ),
  
  # default values
  prototype = list(
    name = NA_character_,
    target = Biostrings::DNAStringSet(""),
    conjugate5 = NA_character_,
    conjugate3 = NA_character_,
    secondary_structure = list(""),
    duplex_structure = list(""),
    dictionary = data.table::data.table(),
    compl_dictionary = data.table::data.table(),
    default_sugar = NA_character_,
    default_backbone = NA_character_
  ),
  
  # check validity of input data
  validity = function(object) {
    errors <- character()
    
    # change base to character
    object@base <- as.character(base(object))
    
    # base, sugar and backbone slots must be the same length
    if (!(length(base(object)) == length(sugar(object)) &
          length(base(object)) == length(backbone(object)))) {
      msg <-
        "@length of base, sugar and backbone slots must be the same"
      errors <- c(errors, msg)
    }
    
    # check on elements amount in slots
    if (xnastringElementsNumber(
      object,
      cond_name = "==1",
      cond_base = "%in% c(1,2)",
      cond_sugar = "%in% c(1,2)",
      cond_backbone = "%in% c(1,2)",
      cond_target = ">0",
      cond_conj5 = "==1",
      cond_conj3 = "==1"
    ) == FALSE) {
      msg <-
        "@XNAString objects must satisfy predefined slots length possibility"
      errors <- c(errors, msg)
    }
    
    #condition on default_sugar and default_backbone
    if (length(default_sugar(object)) > 1 |
        length(default_backbone(object)) > 1 |
        !nchar(default_sugar(object)) %in% c(NA, 1) |
        !nchar(default_backbone(object)) %in% c(NA, 1)) {
      msg <-
        paste("default_sugar and default_backbone must consist",
              " of one letter or be empty",
              sep = '')
      errors <- c(errors, msg)
    }
    
    # condition on sugar and base length
    if (any(nchar(sugar(object)) != nchar(base(object)))) {
      msg <- "@sugar and @base must be the same length"
      errors <- c(errors, msg)
    }
    # condition on backbone length
    if (any(!is.na(backbone(object)) &
            (nchar(backbone(object)) + 1) != nchar(base(object)))) {
      msg <-  "@backbone must be 1 element shorter than base and sugar"
      errors <- c(errors, msg)
    }
    
    # condition on available letters in base dictionary
    unique_base_chars <- unique(unlist(uniqueChars(base(object))))
    
    if (!all(unique_base_chars %in%
             dictionary(object)[type == "base"][["symbol"]])) {
      msg <- sprintf(
        "@base %s %s not present in dictionary.",
        paste(unique_base_chars[!(unique_base_chars %in%
                                    dictionary(object)[type == "base"]
                                  [["symbol"]])],
              collapse = ', '),
        ifelse(length(
          which(unique_base_chars %in%
                  dictionary(object)[type == "base"]
                [["symbol"]] == FALSE)
        ) > 1, 'are', 'is')
      )
      errors <- c(errors, msg)
    }
    
    # # condition on available letters in sugar dictionary
    unique_sugar_chars <- unique(unlist(uniqueChars(sugar(object))))
    
    if (!all(unique_sugar_chars %in%
             dictionary(object)[type == "sugar"][["symbol"]])) {
      msg <- sprintf(
        "@sugar %s %s not present in dictionary.",
        paste(unique_sugar_chars[!(unique_sugar_chars %in%
                                     dictionary(object)[type == "sugar"]
                                   [["symbol"]])],
              collapse = ', '),
        ifelse(length(
          which(unique_sugar_chars %in%
                  dictionary(object)[type == "sugar"]
                [["symbol"]] == FALSE)
        ) > 1, 'are', 'is')
      )
      errors <- c(errors, msg)
    }
    
    # condition on available letters in backbone dictionary
    unique_backbone_chars <-
      unique(unlist(uniqueChars(backbone(object))))
    
    if (!all(unique_backbone_chars %in%
             dictionary(object)[type == "backbone"][["symbol"]])) {
      msg <- sprintf(
        "@backbone %s %s not present in dictionary.",
        paste(unique_backbone_chars[!(unique_backbone_chars %in%
                                        dictionary(object)[type == "backbone"]
                                      [["symbol"]])],
              collapse = ', '),
        ifelse(length(
          which(unique_backbone_chars %in%
                  dictionary(object)[type == "backbone"][["symbol"]] == FALSE)
        ) > 1, 'are', 'is')
      )
      errors <- c(errors, msg)
    }
    
    
    if (length(errors) == 0) {
      TRUE
    } else {
      errors
    }
  }
)


#'
#' XNAString function creates XNAString objects
#' @export
#' @rdname xnastringClass
XNAString <- function(name,
                      base,
                      sugar,
                      backbone,
                      target,
                      conjugate5,
                      conjugate3,
                      secondary_structure,
                      duplex_structure,
                      dictionary,
                      compl_dictionary,
                      default_sugar,
                      default_backbone) {
  return(
    XNAStringMethod(
      name,
      base,
      sugar,
      backbone,
      target,
      conjugate5,
      conjugate3,
      secondary_structure,
      duplex_structure,
      dictionary,
      compl_dictionary,
      default_sugar,
      default_backbone
    )
  )
}


### =========================================================================
### Overwrite XString's show method
### -------------------------------------------------------------------------
#' @rdname xnastringClass
#' @aliases xnastringClass showMethod
#' @export
setMethod("show", "XNAString",
          function(object)
          {
            cat(
              "XNAString object\n",
              "name:       ",
              paste(name(object), collapse = ', '),
              "\n",
              "base:       ",
              paste(base(object), collapse = ', '),
              "\n",
              "sugar:      ",
              paste(sugar(object), collapse = ', '),
              "\n",
              "backbone:   ",
              paste(backbone(object), collapse = ', '),
              "\n",
              "target:     ",
              paste(target(object), collapse = ', '),
              "\n",
              "conjugate5: ",
              paste(conjugate5(object), collapse = ', '),
              "\n",
              "conjugate3: ",
              paste(conjugate3(object), collapse = ', '),
              "\n",
              "secondary_structure: ",
              paste(secondary_structure(object), collapse = ', '),
              "\n",
              "duplex_structure: ",
              paste(duplex_structure(object), collapse = ', '),
              "\n",
              sep = ""
            )
          })



### =========================================================================
### Define the constructor - needed while calling 'new' function
### -------------------------------------------------------------------------
#' @export
#' @rdname xnastringClass
#' @aliases xnastringClass initialize
#' @include setterGetter.R
setMethod(
  "initialize",
  signature  = "XNAString",
  definition = function (.Object,
                         name,
                         base,
                         sugar,
                         backbone,
                         target,
                         conjugate5,
                         conjugate3,
                         secondary_structure,
                         duplex_structure,
                         dictionary,
                         compl_dictionary,
                         default_sugar,
                         default_backbone) {
    # if name not missing, assign it to  name slot
    if (!missing(name)) {
      .Object@name <- name
    }
    # base can not be missing
    .Object@base <- base
    
    # if dictionary not missing, assign it to  dictionary slot
    if (!missing(dictionary)) {
      .Object@dictionary <- dictionary
    } else {
      .Object@dictionary <- xna_dictionary
    }
    
    # if dictionary not missing, assign it to  dictionary slot
    if (!missing(compl_dictionary)) {
      all(c("base", "target") %in% colnames(compl_dictionary)) ||
        stop("base and target columns are required",
             " in dictionary with complementary bases")
      .Object@compl_dictionary <- compl_dictionary
    } else {
      .Object@compl_dictionary <- complementary_bases
    }
    
    # IF BASE CHARACTER
    if (is(base, 'character')) {
      #if sugar is missing, and backbone is missing, sugar 'D', backbone 'O'
      if (missing(sugar) & missing(backbone)) {
        if (missing(default_sugar)) {
          default_sugar <- 'D'
        }
        if (missing(default_backbone)) {
          default_backbone <- 'O'
        }
        .Object@sugar <-
          substring(paste(rep(default_sugar, sum(nchar(
            base
          ))), collapse = ''),
          c(1, cumsum(nchar(base)) + 1)[-(length(cumsum(nchar(base))) +
                                            1)],
          cumsum(nchar(base)))
        .Object@backbone <-
          substring(paste(rep(default_backbone, sum(
            nchar(base) - 1
          )), collapse = ''),
          c(1, cumsum(nchar(base) - 1) + 1)[-(length(cumsum(nchar(base))) +
                                                1)],
          cumsum(nchar(base) - 1))
      }
      if (!missing(sugar) & !missing(backbone)) {
        .Object@sugar <- sugar
        .Object@backbone <- backbone
      }
      #if sugar is missing, fill in the default value
      if (missing(sugar)) {
        if (missing(default_sugar)) {
          default_sugar <- 'D'
        }
        .Object@sugar <-
          substring(paste(rep(default_sugar, sum(nchar(
            base
          ))), collapse = ''),
          c(1, cumsum(nchar(base)) + 1)[-(length(cumsum(nchar(base))) +
                                            1)],
          cumsum(nchar(base)))
      } else {
        .Object@sugar <- sugar
      }
      #if backbone is missing, fill in the default value
      if (missing(backbone)) {
        if (missing(default_backbone)) {
          default_backbone <- 'X'
        }
        .Object@backbone <-
          substring(paste(rep(default_backbone, sum(
            nchar(base) - 1
          )), collapse = ''),
          c(1, cumsum(nchar(base) - 1) + 1)[-(length(cumsum(nchar(base))) +
                                                1)],
          cumsum(nchar(base) - 1))
      } else {
        .Object@backbone <- backbone
      }
      
      # IF BASE DNASTRING
    } else if (is(base, "DNAString") | is(base, "DNAStringSet")) {
      base <- as.character(base)
      if (missing(sugar)) {
        if (missing(default_sugar)) {
          default_sugar <- 'D'
        }
        .Object@sugar <-
          substring(paste(rep(default_sugar, sum(nchar(
            base
          ))), collapse = ''),
          c(1, cumsum(nchar(base)) + 1)[-(length(cumsum(nchar(base))) +
                                            1)],
          cumsum(nchar(base)))
      } else {
        .Object@sugar <- sugar
      }
      if (missing(backbone)) {
        if (missing(default_backbone)) {
          default_backbone <- 'O'
        }
        .Object@backbone <-
          substring(paste(rep(default_backbone, sum(
            nchar(base) - 1
          )), collapse = ''),
          c(1, cumsum(nchar(base) - 1) + 1)[-(length(cumsum(nchar(base))) +
                                                1)],
          cumsum(nchar(base) - 1))
      } else {
        .Object@backbone <- backbone
      }
      
      # IF BASE RNASTRING
    } else if (is(base, "RNAString") | is(base, "RNAStringSet")) {
      base <- as.character(base)
      if (missing(sugar)) {
        if (missing(default_sugar)) {
          default_sugar <- 'R'
        }
        .Object@sugar <-
          substring(paste(rep(default_sugar, sum(nchar(
            base
          ))), collapse = ''),
          c(1, cumsum(nchar(base)) + 1)[-(length(cumsum(nchar(base))) +
                                            1)],
          cumsum(nchar(base)))
      } else {
        .Object@sugar <- sugar
      }
      if (missing(backbone)) {
        if (missing(default_backbone)) {
          default_backbone <- 'O'
        }
        backbone(.Object) <-
          substring(paste(rep(default_backbone, sum(
            nchar(base) - 1
          )), collapse = ''),
          c(1, cumsum(nchar(base) - 1) + 1)[-(length(cumsum(nchar(base))) +
                                                1)],
          cumsum(nchar(base) - 1))
      } else {
        .Object@backbone <- backbone
      }
    }
    
    # if missing target and if dictionaries match - default value
    type <- NULL
    any(colnames(.Object@dictionary) == 'type') ||
      stop("HELM-symbol dictionary must include type column.")
    if_default_target <- TRUE
    if (nrow(.Object@compl_dictionary) != 0) {
      if (!all(.Object@dictionary[type == "base"][["symbol"]] %in%
               .Object@compl_dictionary[["base"]])) {
        print(
          paste(
            "All bases from HELM-symbol dictionary should be present",
            " in complementary bases dictionary. That is not the case",
            " so default target is empty.",
            sep = ''
          )
        )
        if_default_target <- FALSE
      }
    }
    
    if (missing(target)) {
      if (if_default_target) {
        .Object@target <-
          Biostrings::DNAStringSet(XNAString::XNAReverseComplement(.Object))
      }
    } else {
      .Object@target <- Biostrings::DNAStringSet(target)
    }
    
    # if conjugate5 not missing, assign it to  conjugate5 slot
    if (!missing(conjugate5)) {
      .Object@conjugate5 <- conjugate5
    }
    # if conjugate3 not missing, assign it to  conjugate3 slot
    if (!missing(conjugate3)) {
      .Object@conjugate3 <- conjugate3
    }
    # if secondary_structure not missing, assign it to secondary_structure slot
    if (!missing(secondary_structure)) {
      .Object@secondary_structure <- secondary_structure
    } else {
      # for single stranded molecules
      if ((
        is(.Object@base, 'character')  && xnastringElementsNumber(
          .Object,
          cond_base = '==1',
          cond_sugar = '==1',
          cond_backbone = '==1'
        ) == TRUE
      ) ||
      is(.Object@base, 'DNAString') ||
      is(.Object@base, 'RNAString')) {
        .Object@secondary_structure <- predictMfeStructure(.Object)
      }
    }
    # if duplex_structure not missing, assign it to duplex_structure slot
    # if duplex_structure not missing, assign it to duplex_structure slot
    if (!missing(duplex_structure)) {
      .Object@duplex_structure <- duplex_structure
    } else {
      # for double stranded molecules
      if ((
        is(.Object@base, 'character') && xnastringElementsNumber(
          .Object,
          cond_base = '<=2',
          cond_sugar = '<=2',
          cond_backbone = '<=2'
        ) == TRUE
      ) ||
      is(.Object@base, 'DNAStringSet') ||
      is(.Object@base, 'RNAStringSet') ||
      is(.Object@base, 'DNAString') ||
      is(.Object@base, 'RNAString')) {
        duplex_structure(.Object) <- predictDuplexStructure(.Object)
      }
    }
    
    if (!missing(default_sugar)) {
      .Object@default_sugar <- default_sugar
    }
    if (!missing(default_backbone)) {
      .Object@default_backbone <- default_backbone
    }
    
    # call of the inspector
    validObject(.Object)
    return(.Object)
  }
)

#' @rdname xnastringClass
#' @aliases xnastringClass seqtype
setGeneric("seqtype", function(x) {
  standardGeneric("seqtype")
})

#' @rdname xnastringClass
#' @aliases xnastringClass seqtype
setMethod("seqtype", "XNAString", function(x) {
  "B"
})
