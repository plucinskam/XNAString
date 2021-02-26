context("matchPattern")

s1 <-
  XNAString::XNAString(
    base = Biostrings::DNAString('GCGGAGAGAGCACAGATACA'),
    sugar = 'FODDDDDDDDDDDDDDDDDD',
    target = Biostrings::DNAStringSet("GGCGGAGAGAGCACAGATACA")
  )
s2 <-
  XNAString::XNAString(base = 'GCGGAGAGAGCACAGATACA',
                       sugar = 'FODDDDDDDDDDDDDDDDDD',
                       target = Biostrings::DNAStringSet(c(
                         "GGCGGAGAGAGCACAGATACA", "GGCGGAGAGAGCACAGATACA"
                       )))

### =========================================================================
### Test matchPattern()
### -------------------------------------------------------------------------

testthat::test_that("XNAMatchPattern works for single target and subject", {
  testthat::expect_error(
          XNAString::XNAMatchPattern(s1, 
                                  "GGCGGAGAGAGCACAGATACAGGCGGAGAGAGCACAGATACA"),
                                   NA)
})

testthat::test_that("XNAMatchPattern works for XNAString with multiple targets 
                    and takes first as a default",
                    {
                      testthat::expect_error(
                                  XNAString::XNAMatchPattern(s2, 
                                  "GGCGGAGAGAGCACAGATACAGGCGGAGAGAGCACAGATACA"),
                                  NA)
                      
testthat::test_that("XNAMatchPattern works for XNAString with multiple targets and takes first as a default",
                    {
                      testthat::expect_error(
                        XNAString::XNAMatchPattern(s2, 
                                              "GGCGGAGAGAGCACAGATACAGGCGGAGAGAGCACAGATACA"),
                        NA)
                    })

testthat::test_that("XNAMatchPattern works for XNAString with multiple targets and takes first as a default",
                    {
                      seq <-
                        Biostrings::DNAString("GGCGGAGAGAGCACAGATACAGGCGGAGAGAGCACAGATACA")
                       r <- XNAString::XNAMatchPattern(s2, seq)
                       testthat::expect_equal(r@ranges@start, c(1,22))
                       testthat::expect_equal(r@ranges@width, c(21,21))
                    })

})
### =========================================================================
### Test vmatchPattern()
### -------------------------------------------------------------------------

testthat::test_that("XNAVmatchPattern works for single target and subject", {
  testthat::expect_error(
          XNAString::XNAVmatchPattern(s1, 
                                  "GGCGGAGAGAGCACAGATACAGGCGGAGAGAGCACAGATACA"),
                         NA)
})

testthat::test_that("XNAVmatchPattern works for single target and 
                    multiple subjects", {
  testthat::expect_error(XNAString::XNAVmatchPattern(
        s1,
        c(
         "GGCGGAGAGAGCACAGATACAGGCGGAGAGAGCACAGATACA",
         "GGCGGAGAGAGCACAGATACAGGCGGAGAGAGCACAGATACA"
        )
      ), NA)
  
})

testthat::test_that("XNAVmatchPattern works for XNAString with multiple targets", {
  testthat::expect_error(XNAString::XNAVmatchPattern(s2, 
                                "GGCGGAGAGAGCACAGATACAGGCGGAGAGAGCACAGATACA"),
                        NA)
  
  testthat::expect_error(XNAString::XNAVmatchPattern(s2,
                                  "GGCGGAGAGAGCACAGATACAGGCGGAGAGAGCACAGATACA",
                                   target.number = 2),
                         NA)

})


testthat::test_that("XNAVmatchPattern works for single target and DNAStringSet as a subject",
                    {
                      seq <- Biostrings::DNAStringSet(
                        c(
                          "GGCGGAGAGAGCACAGATACAGGCGGAGAGAGCACAGATACA",
                          "GGCGGAGAGAGCACAGATACAGGCGGAGAGAGCACAGATACA"
                        )
                      )
                      
                      p1 <- XNAString::XNAVmatchPattern(s1,
                                                     seq)
                      
                      p2 <- XNAString::XNAVmatchPattern(
                        s1,
                        c(
                          "GGCGGAGAGAGCACAGATACAGGCGGAGAGAGCACAGATACA",
                          "GGCGGAGAGAGCACAGATACAGGCGGAGAGAGCACAGATACA"
                        )
                      )
                      
                      testthat::expect_equal(p1, p2)
                      
                    })


testthat::test_that("XNAMatchPattern works for chromosome from BSgenome", {
  s3 <-
    XNAString::XNAString(
      base = 'GCGGAGAGAGCACAGATACA',
      sugar = 'FODDDDDDDDDDDDDDDDDD',
      target = Biostrings::DNAStringSet(c("AAAAGCTTTACAAAATCCAAGATC", "GGCGGAGAGAGCACAGATACA"))
    )
  
  chrom <- BSgenome.Hsapiens.UCSC.hg38::BSgenome.Hsapiens.UCSC.hg38$chr1
  result <- XNAString::XNAMatchPattern(s3, chrom)
  testthat::expect_equal(result@ranges@start, c(450000, 684976))
  testthat::expect_equal(result@ranges@width, c(24, 24))
  
})

testthat::test_that("XNAVmatchPattern works for chromosome from BSgenome", {
  s3 <-
    XNAString::XNAString(
      base = 'GCGGAGAGAGCACAGATACA',
      sugar = 'FODDDDDDDDDDDDDDDDDD',
      target = Biostrings::DNAStringSet(c("AAAAGCTTTACAAAATCCAAGATC", "GGCGGAGAGAGCACAGATACA"))
    )
  
  genome <- BSgenome.Hsapiens.UCSC.hg38::BSgenome.Hsapiens.UCSC.hg38
  result <- XNAString::XNAVmatchPattern(s3, genome)
  testthat::expect_equal(result[1]@ranges@start, 450000)
  testthat::expect_equal(result[1]@ranges@width, 24)
  
})
