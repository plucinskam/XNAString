
### =========================================================================
### The XNAPairwiseAlignment() generic & related functions based on Biostrings
### -------------------------------------------------------------------------

#' Pairwise alignment methods for XNAString object
#'
#' This function performs pairwise alignment for sequences stored in target slot
#'  of XNAString object with subject
#'
#' @param pattern XNAString object, pattern taken from target slot.
#' @param subject a character vector of length 1, an XString, or an XStringSet 
#' object of length 1.
#' @param type type of alignment. One of "global", "local", "overlap", 
#' "global-local", and "local-global" where "global" = align whole strings 
#' with end gap penalties, "local" = align string fragments, "overlap" = align 
#' whole strings without end gap penalties, "global-local" = align whole strings
#'  in pattern with consecutive subsequence of subject, "local-global" = align 
#'  consecutive subsequence of pattern with whole strings in subject.
#' @param substitutionMatrix substitution matrix representing the fixed 
#' substitution scores for an alignment. It cannot be used in conjunction 
#' with patternQuality and subjectQuality arguments.
#' @param fuzzyMatrix fuzzy match matrix for quality-based alignments. 
#' It takes values between 0 and 1; where 0 is an unambiguous mismatch, 1 
#' is an unambiguous match, and values in between represent a fraction of 
#' "matchiness". 
#' @param gapOpening the cost for opening a gap in the alignment.
#' @param gapExtension the incremental cost incurred along the length of the 
#' gap in the alignment.
#' @param scoreOnly logical to denote whether or not to return just the scores 
#' of the optimal pairwise alignment.
#' @param ... optional arguments to generic function to support additional methods
#'
#' @importMethodsFrom Biostrings pairwiseAlignment
#' @include xnaStringClass.R
#' 
#' @rdname XNAPairwiseAlignment
#' @name XNAPairwiseAlignment
#' @export

setGeneric("XNAPairwiseAlignment",
           function(pattern,
                    subject,
                    ...)
             standardGeneric("XNAPairwiseAlignment"))

#' @rdname XNAPairwiseAlignment
setMethod("XNAPairwiseAlignment", c("XNAString", "character"),
          function(pattern,
                   subject,
                   type = "global",
                   substitutionMatrix = NULL,
                   fuzzyMatrix = NULL,
                   gapOpening = 10,
                   gapExtension = 4,
                   scoreOnly = FALSE) {
            pattern <- pattern@target
            
            pattern_seqtype <- "B"
            subject_seqtype <- "B"
            
            if (pattern_seqtype == "B")
              pattern_seqtype <- subject_seqtype
            if (subject_seqtype == "B")
              subject_seqtype <- pattern_seqtype
            
            XStringSet <- utils::getFromNamespace("XStringSet", "Biostrings")
            
            pattern <- XStringSet(pattern_seqtype, pattern)
            subject <- XStringSet(subject_seqtype, subject)
            
            mpi.XStringSet.pairwiseAlignment <- utils::getFromNamespace("mpi.XStringSet.pairwiseAlignment", "Biostrings")
            
            mpi.XStringSet.pairwiseAlignment(
              pattern,
              subject,
              type = type,
              substitutionMatrix = substitutionMatrix,
              gapOpening = gapOpening,
              gapExtension = gapExtension,
              scoreOnly = scoreOnly
            )
          })
