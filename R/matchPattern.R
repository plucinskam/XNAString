

### =========================================================================
### The XNAMatchPattern() generic & related functions
### -------------------------------------------------------------------------
#' Finds pattern in reference sequence
#'
#' This is function finding all the occurrences of a given pattern
#'  (typically short) in a (typically long) reference sequence
#'
#' @param pattern XNAString object with non-empty target slot
#' @param subject string or DNAString object
#' @param target.number numeric - if target is a multi-element vector,
#' then specify which element in use. 1 is the default
#' @param max.mismatch The maximum number of mismatching letters allowed.
#' If non-zero, an algorithm that supports inexact matching is used.
#' @param min.mismatch The minimum number of mismatching letters allowed.
#' If non-zero, an algorithm that supports inexact matching is used.
#' @param with.indels If TRUE then indels are allowed. In that case,
#' min.mismatch must be 0 and max.mismatch is interpreted as the maximum
#' "edit distance" allowed between the pattern and a match. Note that in order
#' to avoid pollution by redundant matches, only the "best local matches" are
#' returned. Roughly speaking, a "best local match" is a match that is locally
#' both the closest (to the pattern P) and the shortest.
#' @param fixed If TRUE (the default), an IUPAC ambiguity code in the pattern
#' can only match the same code in the subject, and vice versa. If FALSE, an
#' IUPAC ambiguity code in the pattern can match any letter in the subject that
#' is associated with the code, and vice versa.
#' @param algorithm One of the following: "auto", "naive-exact",
#' "naive-inexact", "boyer-moore", "shift-or" or "indels".
#'
#' @include xnaStringClass.R
#' @rdname XNAMatchPattern
#' @name XNAMatchPattern
#' @return an \link{XStringViews} object for \code{matchPattern}.
#' @export

setGeneric("XNAMatchPattern",
           signature = c("pattern", "subject"),
           function(pattern,
                    subject,
                    target.number = 1,
                    max.mismatch = 0,
                    min.mismatch = 0,
                    with.indels = FALSE,
                    fixed = TRUE,
                    algorithm = "auto") {
             standardGeneric("XNAMatchPattern")
           })

#' @rdname XNAMatchPattern
setMethod("XNAMatchPattern", c("XNAString", "character"),
          function(pattern,
                   subject,
                   target.number = 1,
                   max.mismatch = 0,
                   min.mismatch = 0,
                   with.indels = FALSE,
                   fixed = TRUE,
                   algorithm = "auto") {
            if (any(nchar(as.character(pattern@target)) == 0)) {
              stop(
                "pattern must be a XNAString object with non-empty ",
                "DNAStringSet as a target for this algorithm"
              )
            }
            
            if (length(subject) > 1 || nchar(subject) == 0) {
              stop(
                "subject must be a single (non-empty) ",
                "string as a target for this algorithm. ",
                "Please use vmatchPattern."
              )
            }
            
            XString.matchPattern <-
              utils::getFromNamespace(".XString.matchPattern", "Biostrings")
            
            XString.matchPattern(
              as.character(pattern@target[target.number]),
              subject,
              max.mismatch,
              min.mismatch,
              with.indels,
              fixed,
              algorithm
            )
          })

#' @rdname XNAMatchPattern
setMethod("XNAMatchPattern", c("XNAString", "XString"),
          function(pattern,
                   subject,
                   target.number = 1,
                   max.mismatch = 0,
                   min.mismatch = 0,
                   with.indels = FALSE,
                   fixed = TRUE,
                   algorithm = "auto") {
            if (nchar(as.character(pattern@target[target.number])) == 0) {
              stop(
                "pattern must be a XNAString object with non-empty ",
                "DNAString as a target for this algorithm"
              )
            }
            
            if (subject@length == 0) {
              stop(
                "subject must be a single (non-empty) ",
                "string as a target for this algorithm. ",
                "Please use vmatchPattern."
              )
            }
            
            XString.matchPattern <-
              utils::getFromNamespace(".XString.matchPattern", "Biostrings")
            
            XString.matchPattern(
              pattern@target[[target.number]],
              subject,
              max.mismatch,
              min.mismatch,
              with.indels,
              fixed,
              algorithm
            )
          })





### =========================================================================
### The XNAVmatchPattern() generic & related functions
### -------------------------------------------------------------------------

#' This is function finding all the occurrences of a given pattern
#'  (typically short) in a (typically long) set of reference sequences.
#' @importClassesFrom BSgenome BSgenome
#' @importFrom BSgenome bsapply seqnames seqinfo
#' @importFrom IRanges IRangesList
#' @importClassesFrom S4Vectors Rle
#' @importFrom S4Vectors runValue
#' @importFrom GenomicRanges GRanges
#' @include xnaStringClass.R
#'
#' @param pattern XNAString object with non-empty target slot
#' @param subject string, string vector or
#'  DNAString / DNAStringSet / chromosome from BSgenome  object
#' @param target.number numeric - if target is a multi-element vector,
#' then specify which element in use. 1 is the default
#' @param max.mismatch The maximum number of mismatching letters allowed.
#' If non-zero, an algorithm that supports inexact matching is used.
#' @param min.mismatch The minimum number of mismatching letters allowed.
#' If non-zero, an algorithm that supports inexact matching is used.
#' @param with.indels If TRUE then indels are allowed. In that case,
#' min.mismatch must be 0 and max.mismatch is interpreted as the maximum
#' "edit distance" allowed between the pattern and a match. Note that in order
#' to avoid pollution by redundant matches, only the "best local matches" are
#' returned. Roughly speaking, a "best local match" is a match that is locally
#' both the closest (to the pattern P) and the shortest.
#' @param fixed If TRUE (the default), an IUPAC ambiguity code in the pattern
#' can only match the same code in the subject, and vice versa. If FALSE, an
#' IUPAC ambiguity code in the pattern can match any letter in the subject that
#' is associated with the code, and vice versa.
#' @param algorithm One of the following: "auto", "naive-exact",
#' "naive-inexact", "boyer-moore", "shift-or" or "indels".
#' @param exclude A character vector with strings that will be used to filter
#' out chromosomes whose names match these strings. Needed for BSParams object
#' if subject is a chromosome object from BSgenome
#' @param maskList A named logical vector of maskStates preferred when used with
#' a BSGenome object. When using the bsapply function, the masks will be set to
#' the states in this vector.
#' @param userMask An IntegerRangesList, containing a mask to be applied
#'  to each chromosome.
#' @param invertUserMask Whether the userMask should be inverted.
#'
#' @return An \link{MIndex} object for \code{vmatchPattern}.
#' @rdname XNAVmatchPattern
#' @name XNAVmatchPattern
#'
#' @export

setGeneric("XNAVmatchPattern",
           signature = c("pattern", "subject"),
           function(pattern,
                    subject,
                    target.number = 1,
                    max.mismatch = 0,
                    min.mismatch = 0,
                    with.indels = FALSE,
                    fixed = TRUE,
                    algorithm = "auto",
                    exclude = "",
                    maskList = logical(0),
                    userMask = IRanges::IRangesList(),
                    invertUserMask = FALSE) {
             standardGeneric("XNAVmatchPattern")
           })

#' @rdname XNAVmatchPattern
setMethod("XNAVmatchPattern", c("XNAString", "character"),
          function(pattern,
                   subject,
                   target.number = 1,
                   max.mismatch = 0,
                   min.mismatch = 0,
                   with.indels = FALSE,
                   fixed = TRUE,
                   algorithm = "auto") {
            if (nchar(as.character(pattern@target[target.number])) == 0) {
              stop(
                "pattern must be a XNAString object with non-empty ",
                "string as a target for this algorithm"
              )
            }
            
            XStringSet.vmatchPattern <-
              utils::getFromNamespace(".XStringSet.vmatchPattern", "Biostrings")
            
            XStringSet.vmatchPattern(
              as.character(pattern@target[target.number]),
              subject,
              max.mismatch,
              min.mismatch,
              with.indels,
              fixed,
              algorithm
            )
          })

#' @rdname XNAVmatchPattern
setMethod("XNAVmatchPattern", c("XNAString", "XStringSet"),
          function(pattern,
                   subject,
                   target.number = 1,
                   max.mismatch = 0,
                   min.mismatch = 0,
                   with.indels = FALSE,
                   fixed = TRUE,
                   algorithm = "auto") {
            if (nchar(as.character(pattern@target[target.number])) == 0) {
              stop(
                "pattern must be a XNAString object with non-empty ",
                "string as a target for this algorithm"
              )
            }
            
            XStringSet.vmatchPattern <-
              utils::getFromNamespace(".XStringSet.vmatchPattern", "Biostrings")
            
            XStringSet.vmatchPattern(
              pattern@target[[target.number]],
              subject,
              max.mismatch,
              min.mismatch,
              with.indels,
              fixed,
              algorithm
            )
          })


#' @rdname XNAVmatchPattern
setMethod("XNAVmatchPattern", c("XNAString", "BSgenome"),
          function(pattern,
                   subject,
                   target.number = 1,
                   max.mismatch = 0,
                   min.mismatch = 0,
                   with.indels = FALSE,
                   fixed = TRUE,
                   algorithm = "auto",
                   exclude = "",
                   maskList = logical(0),
                   userMask = IRanges::IRangesList(),
                   invertUserMask = FALSE) {
            matchFUN <- function(posPattern,
                                 negPattern,
                                 chr,
                                 seqlengths,
                                 max.mismatch = max.mismatch,
                                 min.mismatch = min.mismatch,
                                 with.indels = with.indels,
                                 fixed = fixed,
                                 algorithm = algorithm) {
              posMatches <-
                Biostrings::matchPattern(
                  pattern = posPattern,
                  subject = chr,
                  max.mismatch = max.mismatch,
                  min.mismatch = min.mismatch,
                  with.indels = with.indels,
                  fixed = fixed,
                  algorithm = algorithm
                )
              
              negMatches <-
                Biostrings::matchPattern(
                  pattern = negPattern,
                  subject = chr,
                  max.mismatch = max.mismatch,
                  min.mismatch = min.mismatch,
                  with.indels = with.indels,
                  fixed = fixed,
                  algorithm = algorithm
                )
              
              COUNTER <<- COUNTER + 1L
              seqnames <- names(seqlengths)
              
              GenomicRanges::GRanges(
                seqnames =
                  S4Vectors::Rle(
                    factor(seqnames[COUNTER], levels = seqnames),
                    length(posMatches) + length(negMatches)
                  ),
                ranges =
                  c(as(posMatches, "IRanges"), as(negMatches, "IRanges")),
                strand =
                  S4Vectors::Rle(BSgenome::strand(c("+", "-")),
                                 c(
                                   length(posMatches), length(negMatches)
                                 )),
                seqlengths = seqlengths
              )
            }
            
            if (nchar(as.character(pattern@target[[target.number]])) == 0) {
              stop(
                "pattern must be a XNAString object with non-empty ",
                "string as a target for this algorithm"
              )
            }
            
            pattern <- pattern@target[[target.number]]
            
            normargAlgorithm <-
              utils::getFromNamespace("normargAlgorithm", "Biostrings")
            
            algorithm <- normargAlgorithm(algorithm)
            
            isCharacterAlgo <-
              utils::getFromNamespace("isCharacterAlgo", "Biostrings")
            
            if (isCharacterAlgo(algorithm)) {
              stop("'subject' must be a single (non-empty) string ",
                   "for this algorithm")
            }
            
            normargPattern <-
              utils::getFromNamespace("normargPattern", "Biostrings")
            pattern <-
              normargPattern(pattern, Biostrings::DNAStringSet())
            
            normargMaxMismatch <-
              utils::getFromNamespace("normargMaxMismatch", "Biostrings")
            max.mismatch <-
              normargMaxMismatch(max.mismatch)
            
            normargMinMismatch <-
              utils::getFromNamespace("normargMinMismatch", "Biostrings")
            min.mismatch <-
              normargMinMismatch(min.mismatch, max.mismatch)
            
            normargWithIndels <-
              utils::getFromNamespace("normargWithIndels", "Biostrings")
            with.indels <-
              normargWithIndels(with.indels)
            
            normargFixed <-
              utils::getFromNamespace("normargFixed", "Biostrings")
            fixed <-
              normargFixed(fixed, Biostrings::DNAStringSet())
            
            posPattern <- pattern
            
            negPattern <- Biostrings::reverseComplement(posPattern)
            
            bsParams <-
              new(
                "BSParams",
                X = subject,
                FUN = matchFUN,
                exclude = exclude,
                simplify = FALSE,
                maskList = logical(0),
                userMask = userMask,
                invertUserMask = invertUserMask
              )
            
            COUNTER <- 0L
            lengths <- BSgenome::seqinfo(subject)@seqlengths
            names <- BSgenome::seqnames(subject)
            names(lengths) <- names
            seqlengths <- lengths
            
            matches <-
              BSgenome::bsapply(
                bsParams,
                posPattern = posPattern,
                negPattern = negPattern,
                seqlengths = seqlengths,
                max.mismatch = max.mismatch,
                min.mismatch = min.mismatch,
                with.indels = with.indels,
                fixed = fixed,
                algorithm = algorithm
              )
            
            nms <-
              factor(names(matches), levels = names(seqlengths))
            nms <-
              nms[unlist(lapply(matches, length), use.names = FALSE) > 0]
            matches <- do.call(c, unname(matches))
            S4Vectors::runValue(BSgenome::seqnames(matches)) <- nms
            matches
          })
