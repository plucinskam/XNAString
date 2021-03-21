


### =========================================================================
### The XNAMatchPDict() generic & related functions
### -------------------------------------------------------------------------
#' Find set of patterns in reference sequence
#'
#' This is function finding all the occurrences of a given set of patterns
#'  (typically short) in a (typically long) reference sequence
#'
#' @param pdict	XNAString object, target slot taken as pdict
#'  object from Biostrings
#' @param subject	string containing sequence
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
#' @param verbose TRUE or FALSE.
#'
#' @include xnaStringClass.R
#' @return an \link{MIndex} object of length \code{M}, and \code{countPDict}
#'  an integer vector of length \code{M}.
#' @rdname XNAMatchPDict
#' @name XNAMatchPDict
#' @export

setGeneric("XNAMatchPDict",
  signature = c("pdict", "subject"),
  function(pdict,
           subject,
           max.mismatch = 0,
           min.mismatch = 0,
           with.indels = FALSE,
           fixed = TRUE,
           algorithm = "auto",
           verbose = FALSE) {
    standardGeneric("XNAMatchPDict")
  }
)

#' @rdname XNAMatchPDict
setMethod(
  "XNAMatchPDict", c("XNAString", "character"),
  function(pdict,
           subject,
           max.mismatch = 0,
           min.mismatch = 0,
           with.indels = FALSE,
           fixed = TRUE,
           algorithm = "auto",
           verbose = FALSE) {
    pattern_dictionary <- Biostrings::PDict(pdict@target)
    subject <- Biostrings::DNAString(subject)

    matchPDict <-
      utils::getFromNamespace(".matchPDict", "Biostrings")

    matchPDict(
      pdict = pattern_dictionary,
      subject = subject,
      max.mismatch,
      min.mismatch,
      with.indels,
      fixed,
      algorithm,
      verbose
    )
  }
)

#' @rdname XNAMatchPDict
setMethod(
  "XNAMatchPDict", c("XNAString", "XString"),
  function(pdict,
           subject,
           max.mismatch = 0,
           min.mismatch = 0,
           with.indels = FALSE,
           fixed = TRUE,
           algorithm = "auto",
           verbose = FALSE) {
    pattern_dictionary <- Biostrings::PDict(pdict@target)
    subject <- Biostrings::DNAString(subject)

    matchPDict <-
      utils::getFromNamespace(".matchPDict", "Biostrings")

    matchPDict(
      pdict = pattern_dictionary,
      subject = subject,
      max.mismatch,
      min.mismatch,
      with.indels,
      fixed,
      algorithm,
      verbose
    )
  }
)
