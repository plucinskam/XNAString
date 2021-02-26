testthat::test_that(desc = "checks if XNAStringSet object creation works",
                    code = {
                      my_dic <-
                        data.table::data.table(
                          type = c(rep('base', 3), 
                                   rep('sugar', 2), 
                                   rep('backbone', 3)),
                          symbol = c('G', 'E', 'A', 'F', 'O', 'S', 'B', 'X')
                        )
                      obj1 <-
                        XNAString(
                          name = 'a',
                          base = 'GGE',
                          sugar = 'FFO',
                          backbone = 'SB',
                          dictionary = my_dic
                        )
                      obj2 <-
                        XNAString(
                          name = 'b',
                          base = 'GGG',
                          sugar = 'FFO',
                          backbone = 'SB',
                          dictionary = my_dic
                        )
                      obj3 <-
                        XNAString(
                          name = 'c',
                          base = 'EEE',
                          sugar = 'FFO',
                          dictionary = my_dic
                        )
                      obj4 <-
                        XNAString(
                          name = 'c',
                          base = c('EEE', 'GEG'),
                          sugar = c('FFO', 'OOO'),
                          dictionary = my_dic
                        )
                      obj5 <-
                        XNAString(
                          name = 'd',
                          base = Biostrings::DNAStringSet(c('AAA', 'GAG')),
                          sugar = c('FFO', 'OOO'),
                          backbone = c('SS', 'SS'),
                          dictionary = my_dic
                        )
                      # object creation - expect no error
                      testthat::expect_error(
                        XNAStringSet(
                          objects = list(obj1, obj2, obj3, obj4, obj5)), NA)
                      
                      # object creation - expect an error
                      testthat::expect_error(
                        XNAStringSet(objects = list(obj1, obj2, 1)))
                      
                      setObj <-
                        XNAStringSet(objects = list(obj1, obj2, obj3, obj4))
                      
                      # check [] method
                      testthat::expect_equal(setObj[[1]]@base, 'GGE')
                      
                      # check set2List method and extraction operators
                      setList <- set2List(setObj)
                      testthat::expect_equal(class(setList[1][[1]])[[1]], 
                                            'XNAStringSet')
                      
                      testthat::expect_equal(class(setList[[1]][[1]])[[1]], 
                                            'XNAString')
                    })



testthat::test_that(desc = "check set2Dt function",
                    code = {
                      my_dic <-
                        data.table::data.table(
                          type = c(rep('base', 3), 
                                   rep('sugar', 2), 
                                   rep('backbone', 3)),
                          symbol = c('G', 'E', 'A', 'F', 'O', 'S', 'B', 'X')
                        )
                      obj1 <-
                        XNAString(
                          name = 'a',
                          base = 'GGE',
                          sugar = 'FFO',
                          backbone = 'SB',
                          dictionary = my_dic
                        )
                      
                      obj2 <-
                        XNAString(
                          name = 'b',
                          base = 'GGE',
                          sugar = 'OOO',
                          backbone = 'SB',
                          dictionary = my_dic
                        )
                      
                      setObj <-
                        XNAStringSet(objects = list(obj1, obj2))
                      
                      out_ref <-
                        data.table::data.table(
                          name = list('a', 'b'),
                          base = list('GGE', 'GGE'),
                          sugar = list('FFO', 'OOO')
                        )
                      
                      out <- set2Dt(setObj, c("name", "base", "sugar"))
                      
                      testthat::expect_equal(out, out_ref)
                      
                    })



testthat::test_that(desc = "check set2Dt function when base DNAString",
                    code = {
                      my_dic <-
                        data.table::data.table(
                          type = c(rep('base', 3), 
                                   rep('sugar', 2), 
                                   rep('backbone', 3)),
                          symbol = c('G', 'E', 'A', 'F', 'O', 'S', 'B', 'X')
                        )
                      obj1 <-
                        XNAString(
                          name = 'a',
                          base = Biostrings::DNAString('GGA'),
                          sugar = 'FFO',
                          backbone = 'SB',
                          dictionary = my_dic
                        )
                      
                      obj2 <-
                        XNAString(
                          name = 'b',
                          base = 'GGE',
                          sugar = 'OOO',
                          backbone = 'SB',
                          dictionary = my_dic
                        )
                      
                      setObj <-
                        XNAStringSet(objects = list(obj1, obj2))
                      
                      out_ref <-
                        data.table::data.table(
                          name = list('a', 'b'),
                          base = list(Biostrings::DNAString('GGA'), 'GGE'),
                          sugar = list('FFO', 'OOO')
                        )
                      
                      out <- set2Dt(setObj, c("name", "base", "sugar"))
                      
                      testthat::expect_equal(out, out_ref)
                      
                    })



testthat::test_that(desc = "check set2Dt function 2",
                    code = {
                      my_dic <-
                        data.table::data.table(
                          type = c(rep('base', 3), 
                                   rep('sugar', 2), 
                                   rep('backbone', 3)),
                          symbol = c('G', 'E', 'A', 'F', 'O', 'S', 'B', 'X')
                        )
                      obj1 <-
                        XNAString(
                          name = 'a',
                          base = c('EEE', 'GEG'),
                          sugar = c('FFO', 'OOO'),
                          dictionary = my_dic
                        )
                      obj2 <-
                        XNAString(
                          name = 'b',
                          base = c('GGG', 'EEG'),
                          sugar = c('FFO', 'FFF'),
                          dictionary = my_dic
                        )
                      
                      setObj <-
                        XNAStringSet(objects = list(obj1, obj2))
                      
                      out_ref <-
                        data.table::data.table(
                          name = list('a', 'b'),
                          base = list(c("EEE", "GEG"), c("GGG", "EEG")),
                          sugar = list(c("FFO", "OOO"), c("FFO", "FFF"))
                        )
                      
                      out <- set2Dt(setObj, c("name", "base", "sugar"))
                      
                      testthat::expect_equal(out, out_ref)
                      
                    })


testthat::test_that(desc = "checks if creation of XNAStringSet from data.table 
                    (or data.frame) works",
                    code = {
                      
                      dt <- data.table::data.table(base_t = c('TT', 'GG'), 
                                       sugar_t = c('FF', 'FO'), 
                                       backbone_t =c('X', 'X'))
                      
                      out <- dt2Set(dt, "base_t", "sugar_t", "backbone_t")
                      
                      out_ref <- XNAStringSet(
                        objects = list(XNAString(base = c('TT'),
                                                 sugar = c('FF')),
                                       XNAString(base = c('GG'),
                                                 sugar = c('FO'))
                      ))
                      testthat::expect_equal(out, out_ref)
                    })


testthat::test_that(desc = "checks if creation of XNAStringSet from data.table 
                    (or data.frame) works (2)",
                    code = {
                      
                      dt <- data.table::data.table(base= list(c('TT', 'GG'), 
                                                              c('TG', 'GT'), 
                                                              c('TG')), 
                                                   sugar = list(c('FF', 'FO'), 
                                                                c('OO', 'OF'), 
                                                                c('OO')), 
                                                   backbone =list(c('X', 'X'), 
                                                                  c('X', 'X'), 
                                                                  c('X')))
                      
                      out <- dt2Set(dt)
                      
                      out_ref <- XNAStringSet(
                        objects = list(XNAString(base = c('TT', 'GG'),
                                                 sugar = c('FF', 'FO')),
                                       XNAString(base = c('TG', 'GT'),
                                                 sugar = c('OO', 'OF')),
                                       XNAString(base = c('TG'),
                                                 sugar = c('OO'))
                        ))
                      testthat::expect_equal(out, out_ref)
                    })


testthat::test_that(desc = "checks if validation while creating XNAStringSet 
                    from data.table (or data.frame) works",
                    code = {
                      
                      dt <- data.table::data.table(base_t = c('JJ', 'GG'), 
                                       sugar_t = c('FF', 'FO'), 
                                       backbone_t =c('X', 'X'))
                      # expect an error - J does not exist in base dictionary
                      testthat::expect_error(dt2Set(dt, 
                                                    "base_t", 
                                                    "sugar_t", 
                                                    "backbone_t")) 
                      
                    })

testthat::test_that(desc = "check XNAString2XNAStringSet function",
                    code = {
                      
                      obj1 <-
                        XNAString(
                          name = 'a',
                          base = c('EEE', 'GEG'),
                          sugar = c('FFO', 'OOO')
                        )
                      
                      out <- XNAString2XNAStringSet(obj1)
                      out_ref <- "XNAStringSet"
                      
                      testthat::expect_equal(class(out)[[1]], out_ref)
                      
                    })

