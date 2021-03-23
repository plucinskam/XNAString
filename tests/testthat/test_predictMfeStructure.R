context("predictMfeStructure")

testthat::test_that("predictMfeStructure gives expected results if base is character", {

  obj <- XNAString(base = 'GAGAGGGAACCAGGCAGGGACCGCAGACAACA', 
                   sugar = 'FODLMFODLMFODLMFODLMFODLMFFFFFFF')
  res <- XNAString::predictMfeStructure(obj)
  
  testthat::expect_equal(res$structure,
                         ".....((..((.....))..))..........")
  
  testthat::expect_equal(res$mfe,-4.5)
})


testthat::test_that("predictMfeStructure gives expected results if base is DNAString", {
  
  obj <- XNAString(base = Biostrings::DNAString('GAGAGGGAACCAGGCAGGGACCGCAGACAACA'))
  res <- XNAString::predictMfeStructure(obj)
  
  testthat::expect_equal(res$structure,
                         ".....((..((.....))..))..........")
  
  testthat::expect_equal(res$mfe,-4.5)
})



testthat::test_that("predictMfeStructure gives error if double stranded molecule", {
  
  obj <- XNAString(base = c('GAGAGGGAACCAGGCAGGGACCGCAGACAACA', 'GAGAGGGAACCAGGCAGGGACCGCAGACAACA'),
                   sugar = c('FODLMFODLMFODLMFODLMFODLMFFFFFFF', 'FODLMFODLMFODLMFODLMFODLMFFFFFFF'))
  testthat::expect_error(XNAString::predictMfeStructure(obj), "sequence parameter must 1-element character vector, DNAString or RNAString type")

})
