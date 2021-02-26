

testthat::test_that("XNAPairwiseAlignment global works properly", {
  mat <-
    Biostrings::nucleotideSubstitutionMatrix(match = 1,
                                             mismatch = -3,
                                             baseOnly = TRUE)
  s1 <-
    XNAString::XNAString(
      base = 'GCGGAGAGAGCACAGATACA',
      sugar = 'FODDDDDDDDDDDDDDDDDD',
      target = Biostrings::DNAStringSet("GGCGGAGAGAGCACAGATACA")
    )
  testthat::expect_error(
    XNAString::XNAPairwiseAlignment(s1,
                                    "ACCCACACACACACACACACAC",
                                    "global",
                                    substitutionMatrix = mat),
    NA
  )
})

testthat::test_that("XNAPairwiseAlignment local works properly", {
  mat <-
    Biostrings::nucleotideSubstitutionMatrix(match = 1,
                                             mismatch = -3,
                                             baseOnly = TRUE)
  s1 <-
    XNAString::XNAString(
      base = 'GCGGAGAGAGCACAGATACA',
      sugar = 'FODDDDDDDDDDDDDDDDDD',
      target = Biostrings::DNAStringSet("GGCGGAGAGAGCACAGATACA")
    )
  testthat::expect_error(
    
    XNAString::XNAPairwiseAlignment(s1,
                                    "ACCCACACACACACACACACAC",
                                    "local",
                                    substitutionMatrix = mat),
    NA
  )
})

testthat::test_that("XNAPairwiseAlignment works properly with multiple targets", {
  mat <-
    Biostrings::nucleotideSubstitutionMatrix(match = 1,
                                             mismatch = -3,
                                             baseOnly = TRUE)

  s2 <-
    XNAString::XNAString(base = 'GCGGAGAGAGCACAGATACA',
                         sugar = 'FODDDDDDDDDDDDDDDDDD',
                         target = Biostrings::DNAStringSet(c(
                           "GGCGGAGAGAGCACAGATACA", "GGCGGAGAGAGCACAGATACA"
                         )))
  
  
  testthat::expect_error(
    XNAString::XNAPairwiseAlignment(s2,
                                    "ACCCACACACACACACACACAC",
                                    "local",
                                    substitutionMatrix = mat),
    NA
  )
})
