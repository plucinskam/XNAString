context("alphabetFrequency")

testthat::test_that("seqAlphabetFrequency gives corrrect results", {
  unique_letters <- c('A', 'B', 'C')
  seq <- c('AABA')
  
  out_ref_1 <- c(3,1,0)
  out_ref_2 <- unique_letters
  
  out <- seqAlphabetFrequency(unique_letters, seq, as.prob=FALSE)
  out_1 <- unname(out)
  out_2 <- names(out)
  
  testthat::expect_equal(out_1, out_ref_1)
  testthat::expect_equal(out_2, out_ref_2)
})


testthat::test_that("seqVectorAlphabetFrequency gives corrrect results", {
  unique_letters <- c('A', 'B', 'C')
  seq_vec <- c('AABA', 'BBBCCC')
  
  out_ref <- matrix(c(3,1,0,0,3,3), nrow = 2, ncol = 3, byrow = TRUE)
  colnames(out_ref) <- unique_letters
  
  out <- seqVectorAlphabetFrequency(unique_letters, seq_vec, as.prob=FALSE)
  
  testthat::expect_equal(out, out_ref)
  
})


testthat::test_that(
  "alphabetFrequency gives corrrect results for XNAString object", {
    my_dic <-
      data.table::data.table(
        type = c(rep('base', 3), rep('sugar', 2), rep('backbone', 3)),
        symbol = c('G', 'E', 'A', 'F', 'O', 'S', 'B', 'X')
      )
    
    xnastring_obj <- XNAString(name = 'b',
                               base = c('GGEG'),
                               sugar = c('FFOO'),
                               dictionary = my_dic)
    
    out_base_1 <- XNAString::XNAAlphabetFrequency(obj = xnastring_obj, 
                                                  slot = 'base', 
                                                  matrix_nbr = 1)
    out_base_2 <- XNAString::XNAAlphabetFrequency(obj = xnastring_obj, 
                                                  slot =  'base',
                                                  matrix_nbr = 2)
    out_base_3 <- XNAString::XNAAlphabetFrequency(obj = xnastring_obj, 
                                                  slot = 'base', 
                                                  as.prob = TRUE)
    out_base_4 <- XNAString::XNAAlphabetFrequency(obj = xnastring_obj, 
                                                  slot = 'base', 
                                                  letters = c('E', 'C'))
    
    out_ref_base_1 <- matrix(c(0.00, 1.00, 3.00), nrow = 1, ncol = 3, byrow = TRUE)
    colnames(out_ref_base_1) <- sort(my_dic[my_dic$type == 'base'][['symbol']])
    out_ref_base_1 <- formattable::formattable(out_ref_base_1, digits = 2, format = "f")
    
    out_ref_base_2 <- matrix(c(0, 0, 0), nrow = 1, ncol = 3, byrow = TRUE)
    colnames(out_ref_base_2) <- sort(my_dic[my_dic$type == 'base'][['symbol']])
    out_ref_base_2 <- formattable::formattable(out_ref_base_2, digits = 2, format = "f")
    
    out_ref_base_3 <- matrix(c(0, 0.25, 0.75), nrow = 1, ncol = 3, byrow = TRUE)
    colnames(out_ref_base_3) <- sort(my_dic[my_dic$type == 'base'][['symbol']])
    out_ref_base_3 <- formattable::formattable(out_ref_base_3, digits = 2, format = "f")
    
    out_ref_base_4 <- matrix(c(1, 0), nrow = 1, ncol = 2, byrow = TRUE)
    colnames(out_ref_base_4) <- c('E', 'C')
    out_ref_base_4 <- formattable::formattable(out_ref_base_4, digits = 2, format = "f")
    
    testthat::expect_equal(out_base_1, out_ref_base_1)
    testthat::expect_equal(out_base_2, out_ref_base_2)
    testthat::expect_equal(out_base_3, out_ref_base_3)
    testthat::expect_equal(out_base_4, out_ref_base_4)
    
  })


testthat::test_that(
  "alphabetFrequency gives corrrect results for XNAStringSet object", {
    my_dic <-
      data.table::data.table(
        type = c(rep('base', 3), rep('sugar', 2), rep('backbone', 3)),
        symbol = c('G', 'E', 'A', 'F', 'O', 'S', 'B', 'X')
      )
    
    XNAString_obj1 <- XNAString(base = c('AGGE', 'EEEA'), 
                                sugar = c('OOOO', 'OOOO'), 
                                backbone = c('SBS','SBS'),
                                dictionary = my_dic)
    
    XNAString_obj2 <- XNAString(base = c('EGEA'), 
                                sugar = c('FFFO'), 
                                dictionary = my_dic)
    
    XNAStringSet_obj <- XNAStringSet(objects = list(XNAString_obj1, 
                                                    XNAString_obj2))
    
    out_base_1 <- XNAString::XNAAlphabetFrequency(obj = XNAStringSet_obj, 
                                                  slot = 'base', 
                                                  matrix_nbr = 1)
    out_base_2 <- XNAString::XNAAlphabetFrequency(obj = XNAStringSet_obj, 
                                                  slot = 'base', 
                                                  matrix_nbr = 2)
    
    out_ref_base_1 <- matrix(c(1, 1, 2, 1, 2, 1), 
                             nrow = 2, 
                             ncol = 3, 
                             byrow = TRUE)
    colnames(out_ref_base_1) <- sort(my_dic[my_dic$type == 'base'][['symbol']])
    out_ref_base_1 <- formattable::formattable(out_ref_base_1, digits = 2, format = "f")
    
    out_ref_base_2 <- matrix(c(1, 3, 0, 0, 0, 0), 
                             nrow = 2, 
                             ncol = 3, 
                             byrow = TRUE)
    colnames(out_ref_base_2) <- sort(my_dic[my_dic$type == 'base'][['symbol']])
    out_ref_base_2 <- formattable::formattable(out_ref_base_2, digits = 2, format = "f")
    
    testthat::expect_equal(out_base_1, out_ref_base_1)
    testthat::expect_equal(out_base_2, out_ref_base_2)
    
  })





testthat::test_that(
  "alphabetFrequency gives corrrect results for XNAStringSet object if base is not a character", {
    my_dic <-
      data.table::data.table(
        type = c(rep('base', 3), rep('sugar', 2), rep('backbone', 3)),
        symbol = c('G', 'C', 'A', 'F', 'O', 'S', 'O', 'X')
      )
    
    XNAString_obj1 <- XNAString(base = Biostrings::DNAStringSet(c('AGGC', 'CCCA')), 
                                sugar = c('OOOO', 'OOOO'), 
                                backbone = c('SOS','SOS'),
                                dictionary = my_dic)
    
    XNAString_obj2 <- XNAString(base = Biostrings::DNAString(c('CGCA')), 
                                sugar = c('FFFO'), 
                                dictionary = my_dic)
    
    XNAStringSet_obj <- XNAStringSet(objects = list(XNAString_obj1, 
                                                    XNAString_obj2))
    
    out_base_1 <- XNAString::XNAAlphabetFrequency(obj = XNAStringSet_obj, 
                                                  slot = 'base', 
                                                  matrix_nbr = 1)
    out_base_2 <- XNAString::XNAAlphabetFrequency(obj = XNAStringSet_obj, 
                                                  slot = 'base', 
                                                  matrix_nbr = 2)

    out_ref_base_1 <- matrix(c(1, 1, 2, 1, 2, 1), 
                             nrow = 2, 
                             ncol = 3, 
                             byrow = TRUE)
    colnames(out_ref_base_1) <- sort(my_dic[my_dic$type == 'base'][['symbol']])
    out_ref_base_1 <- formattable::formattable(out_ref_base_1, digits = 2, format = "f")
    
    out_ref_base_2 <- matrix(c(1, 3, 0, 0, 0, 0), 
                             nrow = 2, 
                             ncol = 3, 
                             byrow = TRUE)
    colnames(out_ref_base_2) <- sort(my_dic[my_dic$type == 'base'][['symbol']])
    out_ref_base_2 <- formattable::formattable(out_ref_base_2, digits = 2, format = "f")
    
    testthat::expect_equal(out_base_1, out_ref_base_1)
    testthat::expect_equal(out_base_2, out_ref_base_2)
    
  })



