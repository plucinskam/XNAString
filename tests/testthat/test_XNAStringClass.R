testthat::test_that(desc = "checks if XNAString object creation works",
                    code = {
                      # object creation - expect no error
                      testthat::expect_error(XNAString(base = 'GCGG',
                                                       sugar = 'FODD'),
                                             NA)
                      testthat::expect_error(
                        XNAString(
                          base = Biostrings::DNAString('GCGG'),
                          sugar = 'FODD',
                          backbone = 'SSO',
                          target = Biostrings::DNAStringSet('CGCC')
                        ),
                        NA
                      )
                      testthat::expect_error(XNAString(
                        name = c('A'),
                        base = c('GCGG', 'GGGG'),
                        sugar = c('FODD', 'DDDO'),
                        target = Biostrings::DNAStringSet('CGCC')
                      ),
                      NA)
                      testthat::expect_error(
                        XNAString(
                          name = c('A'),
                          base = Biostrings::RNAStringSet(c('GCGG', 'GGGG')),
                          sugar = c('FODD', 'DDDO'),
                          target = Biostrings::DNAStringSet('CGCC')
                        ),
                        NA
                      )
                      
                      # object creation - expect an error
                      testthat::expect_error(
                        XNAString(
                          base = 'GCGG',
                          sugar = 'FODD',
                          backbone = 'SS',
                          target = Biostrings::DNAStringSet('CGCC')
                        )
                      ) # backbone length wrong
                      testthat::expect_error(XNAString(base = 'GCGG',
                                                       sugar = 'FOD'))
                      testthat::expect_error(XNAString(base = 'GCGG',
                                                       sugar = 288))
                      testthat::expect_error(XNAString(
                        name = c('A'),
                        base = c('GCGG', 'GGGG', 'CCCG'),
                        sugar = c('FODD', 'DDDO', 'DDFF'),
                        target = Biostrings::DNAStringSet('CGCC')
                      ))
                      
                      # check if the default value for backbone is created properly
                      obj <-
                        XNAString(base = 'GCGG', sugar = 'FODD')
                      testthat::expect_equal(obj@backbone, 'XXX')
                      testthat::expect_equal(obj@target, Biostrings::DNAStringSet('CCGC'))
                      
                      obj2 <- XNAString(base = c('GCGG', 'CCC'),
                                        sugar = c('FODD', 'DDD'))
                      testthat::expect_equal(obj2@backbone, c('XXX', 'XX'))
                      testthat::expect_equal(obj2@target, Biostrings::DNAStringSet(c('CCGC')))
                      
                      obj3 <-
                        XNAString(base = Biostrings::DNAStringSet(c('GCGG', 'CCC')))
                      testthat::expect_equal(obj3@backbone, c('OOO', 'OO'))
                      testthat::expect_equal(obj3@sugar, c('DDDD', 'DDD'))
                      
                    })

testthat::test_that("checks if XNAString object creation works w Biostrings::DNAString base",
                    {
                      obj <- XNAString(base = Biostrings::DNAString('GCGG'))
                      testthat::expect_equal(obj@sugar, 'DDDD')
                      testthat::expect_equal(obj@backbone, 'OOO')
                      testthat::expect_equal(obj@target, Biostrings::DNAStringSet('CCGC'))
                    })

testthat::test_that("checks if passing own dictionary while creating XNAString object works",
                    {
                      my_dic <-
                        data.table::data.table(
                          type = c(rep('base', 3),
                                   rep('sugar', 2),
                                   rep('backbone', 2)),
                          symbol = c('G', 'E', 'A', 'F', 'O', 'S', 'B')
                        )
                      # object creation - expect no error
                      testthat::expect_error(XNAString(
                        base = 'GGE',
                        sugar = 'FFO',
                        backbone = 'SB',
                        dictionary = my_dic
                      ),
                      NA)
                      # object creation - expect an error
                      testthat::expect_error(XNAString(
                        base = 'GCG',
                        sugar = 'FOD',
                        dictionary = my_dic
                      ))
                    })


testthat::test_that("checks if XNAString object creation with
                    XNAStringFromHelm function works (1)",
                    {
                      # check if the default value for backbone is created
                      # properly
                      obj <-
                        XNAStringFromHelm(helm = "RNA1{[dR](A)P.[dR](A)P.[dR](A)}$$$$V2.0")
                      testthat::expect_equal(obj@backbone, 'OO')
                      testthat::expect_equal(obj@sugar, 'DDD')
                      testthat::expect_equal(obj@base, 'AAA')
                      
                      helm <-
                        "RNA1{[LR](K)[sP].[LR](G)}$$$$V2.0"
                      testthat::expect_error(XNAStringFromHelm(helm),
                                             "Base (K) is not present in dictionary.",
                                             fixed = TRUE)
                      
                    })

testthat::test_that(desc = "checks if XNAString object creation with
                           XNAStringFromHelm function works (2)",
                    code = {
                      # check if the default value for backbone is created
                      # properly
                      obj <-
                        XNAStringFromHelm(helm = "CHEM1{[5gn2c6]}|RNA1{P.[dR](C)P.[dR](A)P.[LR](G)[sP].[LR](A)[sP].[LR](G)[sP].[LR](A)[sP].[dR](A)[sP].[dR](G)[sP].[dR](G)[sP].[dR](C)[sP].[dR](A)[sP].[dR](C)[sP].[dR](A)[sP].[dR](G)[sP].[dR](A)[sP].[LR]([5meC])[sP].[LR](G)[sP].[LR](G)}$CHEM1,RNA1,1:R2-1:R1$$$V2.0",
                                          remove_linker = FALSE)
                      testthat::expect_equal(obj@backbone, 'OOXXXXXXXXXXXXXXX')
                      testthat::expect_equal(obj@sugar, 'DDLLLLDDDDDDDDDLLL')
                      testthat::expect_equal(obj@base, 'CAGAGAAGGCACAGAEGG')
                      testthat::expect_equal(obj@target,
                                             Biostrings::DNAStringSet('CCGTCTGTGCCTTCTCTG'))
                      
                      
                    })

testthat::test_that(desc = "checks if XNAString object creation with
                    XNAStringFromHelm function works (3)",
                    code = {
                      # check if the default value for backbone is created
                      # properly
                      obj <-
                        XNAStringFromHelm(
                          helm = c(
                            "RNA1{[dR](A)P.[dR](A)P.[dR](A)}$$$$V2.0",
                            "RNA1{[dR](T)P.[dR](T)P.[dR](A)}$$$$V2.0"
                          )
                        )[[2]]
                      testthat::expect_equal(obj@backbone, 'OO')
                      testthat::expect_equal(obj@sugar, 'DDD')
                      testthat::expect_equal(obj@base, 'TTA')
                      
                    })


testthat::test_that(desc = "checks if making complementarity dictionary
                    optional works",
                    code = {
                       
                      
                      compl_dict <- data.table::data.table(base = c("A", "J", "K"),
                                                           target = c("A", "B", "C"),
                                                           compl_target = c("A", "J", "K"))

                      dict <-  data.table::data.table(HELM = c(rep('',4)),
                                                     type = c(rep('base',2), 'sugar', 'backbone'),
                                                     symbol = c('T', 'K', 'K', 'K'))

                      expect_that(XNAString(base = "TTKK", 
                                            sugar = 'KKKK', 
                                            backbone = 'KKK',
                                            dictionary = dict,
                                            compl_dictionary = compl_dict),
                                  prints_text("All bases from HELM-symbol dictionary should be present in complementary bases dictionary. That is not the case so default target is empty.")
                      )
                    })