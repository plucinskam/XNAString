context("predictDuplexStructure")

testthat::test_that("predictDuplexStructure gives expected results if base is character (1)", {
  
  obj <- XNAString(base = c('GCGCUUCGCCGCGCGCC', 'GCGCUUCGCCGCGCGCA'),
                   sugar = c('FODLMFODLMFODLMFO', 'FODLMFODLMFODLMFO'),
                   target = '')
  
  res <- XNAString::predictDuplexStructure(obj)
  
  testthat::expect_equal(res$structure,
                         "((((..((..((((...&))))..))..))))...")
  
  testthat::expect_equal(res$mfe, -17.7, tolerance = 1e-5)
})



testthat::test_that("predictDuplexStructure gives expected results if base is character (2)", {
  
  obj <- XNAString(base = c('GCUGGAU', 'GUCCAGU'),
                   sugar = c('FODLMFD', 'FODLMFD'),
                   target = '')
  
  res <- XNAString::predictDuplexStructure(obj)
  
  testthat::expect_equal(res$structure,
                         "(((((((&)))))))")
  
  testthat::expect_equal(res$mfe, -8.7, tolerance = 1e-5)
})




testthat::test_that("predictDuplexStructure gives expected results if base is DNAStringSet", {
  
  obj <- XNAString(base = Biostrings::DNAStringSet(c('GAGAGGGAACCAGGCAGGGACCGCAGACAACA', 'GAGAGGGAACCAGGCAGGGACCGCAGACAACA')))

  res <- XNAString::predictDuplexStructure(obj)
  
  testthat::expect_equal(res$structure,
                         ".....((..((..((.((..((..........&.....))..))..)).))..))..........")
  
  testthat::expect_equal(res$mfe, -14.5, tolerance = 1e-5)
})




testthat::test_that("predictDuplexStructure gives expected results if single stranded molecule (1)", {
  
  obj <- XNAString(base = 'GAGAGGGAACCAGGCAGGGACCGCAGACAACA', 
                   sugar = 'FODLMFODLMFODLMFODLMFODLMFFFFFFF')
  
  res <- XNAString::predictDuplexStructure(obj)
  
  testthat::expect_equal(res$structure,
                         ".....((..((..((.((..((..........&.....))..))..)).))..))..........")
  
  testthat::expect_equal(res$mfe, -14.5, tolerance = 1e-5)
})




testthat::test_that("predictDuplexStructure gives expected results if single stranded molecule (2)", {
  
  obj <- XNAString(base = Biostrings::DNAString('GAGAGGGAACCAGGCAGGGACCGCAGACAACA'), 
                   sugar = 'FODLMFODLMFODLMFODLMFODLMFFFFFFF')
  
  res <- XNAString::predictDuplexStructure(obj)
  
  testthat::expect_equal(res$structure,
                         ".....((..((..((.((..((..........&.....))..))..)).))..))..........")
  
  testthat::expect_equal(res$mfe, -14.5, tolerance = 1e-5)
})


testthat::test_that("predictDuplexStructure gives expect default value", {
  
  obj1 <- XNAString(base = Biostrings::DNAString('GAGAGGGAACCAGGCAGGGACCGCAGACAACA')) 
  
  obj2 <- XNAString(base = Biostrings::DNAStringSet(c('GAGAGGGAACCAGGCAGGGACCGCAGACAACA', 'GAGAGGGAACCAGGCAGGGACCGCAGACAACA')))  
  
  testthat::expect_equal(obj1@duplex_structure, obj2@duplex_structure)
  
})
