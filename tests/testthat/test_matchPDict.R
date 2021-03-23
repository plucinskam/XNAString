


testthat::test_that("XNAMatchPDict works for multiple targets", {
  s2 <-
    XNAString::XNAString(base = 'GCGGAGAGAGCACAGATACA',
                         sugar = 'FODDDDDDDDDDDDDDDDDD',
                         target = Biostrings::DNAStringSet(c(
                           "GGCGGAGAGAGCACAGATACA", "GGCGGAGAGAGCACAGATACA"
                         )))
  o <- XNAString::XNAMatchPDict(s2,
                                "GGCGGAGAGAGCACAGATACAGGGGCGGAGAGAGCACAGATACACGGAGAGAGCACAGATACA")
  testthat::expect_equal(length(o), 2)
})

testthat::test_that("XNAMatchPDict works for multiple targets with base non character", {
  s2 <-
    XNAString::XNAString(base = Biostrings::DNAString('GCGGAGAGAGCACAGATACA'),
                         sugar = 'FODDDDDDDDDDDDDDDDDD',
                         target = Biostrings::DNAStringSet(c(
                           "GGCGGAGAGAGCACAGATACA", "GGCGGAGAGAGCACAGATACA"
                         )))
  o <- XNAString::XNAMatchPDict(s2,
                                "GGCGGAGAGAGCACAGATACAGGGGCGGAGAGAGCACAGATACACGGAGAGAGCACAGATACA")
  testthat::expect_equal(length(o), 2)
})

testthat::test_that("XNAMatchPDict works for multiple targets and DNAString as a subject",
                    {
                      s2 <-
                        XNAString::XNAString(base = 'GCGGAGAGAGCACAGATACA',
                                             sugar = 'FODDDDDDDDDDDDDDDDDD',
                                             target = Biostrings::DNAStringSet(c(
                                               "GGCGGAGAGAGCACAGATACA", "GGCGGAGAGAGCACAGATACA"
                                             )))
                      seq <-
                        Biostrings::DNAString("GGCGGAGAGAGCACAGATACAGGGGCGGAGAGAGCACAGATACACGGAGAGAGCACAGATACA")
                      o <- XNAString::XNAMatchPDict(s2, seq)
                      testthat::expect_equal(length(o), 2)
                    })
