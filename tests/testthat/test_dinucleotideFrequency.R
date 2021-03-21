context("dinucleotideFrequency")

testthat::test_that("seqDinucleotideFrequency gives corrrect results", {
  unique_sets <- c("AB", "BA", "CD")
  seq <- c("ABABAB")

  out_ref_1 <- c(3, 2, 0)
  out_ref_2 <- unique_sets

  out <-
    seqDinucleotideFrequency(c("AB", "BA", "CD"), "ABABAB", as.prob = FALSE)
  out_1 <- unname(out)
  out_2 <- names(out)

  testthat::expect_equal(out_1, out_ref_1)
  testthat::expect_equal(out_2, out_ref_2)
})


testthat::test_that("seqVectorDinucleotideFrequency gives corrrect results", {
  unique_sets <- c("AB", "BA", "CD")
  seq_vec <- c("ABABAB", "ABABCD")

  out_ref <-
    matrix(c(3, 2, 0, 2, 1, 1),
      nrow = 2,
      ncol = 3,
      byrow = TRUE
    )
  colnames(out_ref) <- unique_sets

  out <-
    seqVectorDinucleotideFrequency(c("AB", "BA", "CD"), c("ABABAB", "ABABCD"), as.prob = FALSE)

  testthat::expect_equal(out, out_ref)
})


testthat::test_that("dinucleotideFrequency gives corrrect results for XNAString object", {
  my_dic <-
    data.table::data.table(
      type = c(rep("base", 3), rep("sugar", 2), rep("backbone", 3)),
      symbol = c("G", "E", "A", "F", "O", "S", "B", "X")
    )

  xnastring_obj <- XNAString(
    name = "b",
    base = c("GGEG"),
    sugar = c("FFOO"),
    dictionary = my_dic
  )

  out_base_1 <-
    XNAString::XNADinucleotideFrequency(
      obj = xnastring_obj,
      slot = "base",
      matrix_nbr = 1
    )
  out_base_2 <-
    XNAString::XNADinucleotideFrequency(
      obj = xnastring_obj,
      slot = "base",
      matrix_nbr = 2
    )
  out_base_3 <-
    XNAString::XNADinucleotideFrequency(
      obj = xnastring_obj,
      slot = "base",
      as.prob = TRUE
    )
  out_base_4 <-
    XNAString::XNADinucleotideFrequency(
      obj = xnastring_obj,
      slot = "base",
      double_letters = c("GE", "EG")
    )

  out_ref_base_1 <-
    matrix(
      c(0.00, 0.00, 0.00, 0.00, 0.00, 1.00, 0.00, 1.00, 1.00),
      nrow = 1,
      ncol = 9,
      byrow = TRUE
    )
  colnames(out_ref_base_1) <-
    c("AA", "EA", "GA", "AE", "EE", "GE", "AG", "EG", "GG")
  out_ref_base_1 <- formattable::formattable(out_ref_base_1, digits = 2, format = "f")

  out_ref_base_2 <-
    matrix(
      c(0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00),
      nrow = 1,
      ncol = 9,
      byrow = TRUE
    )
  colnames(out_ref_base_2) <-
    c("AA", "EA", "GA", "AE", "EE", "GE", "AG", "EG", "GG")
  out_ref_base_2 <- formattable::formattable(out_ref_base_2, digits = 2, format = "f")

  out_ref_base_3 <-
    matrix(
      c(0.00, 0.00, 0.00, 0.00, 0.00, 0.33, 0.00, 0.33, 0.33),
      nrow = 1,
      ncol = 9,
      byrow = TRUE
    )
  colnames(out_ref_base_3) <-
    c("AA", "EA", "GA", "AE", "EE", "GE", "AG", "EG", "GG")
  out_ref_base_3 <- formattable::formattable(out_ref_base_3, digits = 2, format = "f")

  out_ref_base_4 <-
    matrix(c(1.00, 1.00),
      nrow = 1,
      ncol = 2,
      byrow = TRUE
    )
  colnames(out_ref_base_4) <- c("GE", "EG")
  out_ref_base_4 <- formattable::formattable(out_ref_base_4, digits = 2, format = "f")

  testthat::expect_equal(out_base_1, out_ref_base_1)
  testthat::expect_equal(out_base_2, out_ref_base_2)
  testthat::expect_equal(out_base_3, out_ref_base_3, tolerance = 1e-2)
  testthat::expect_equal(out_base_4, out_ref_base_4)
})





testthat::test_that("dinucleotideFrequency gives corrrect results for XNAString object (2)", {
  my_dic <-
    data.table::data.table(
      type = c(rep("base", 3), rep("sugar", 2), rep("backbone", 3)),
      symbol = c("G", "E", "A", "F", "O", "S", "B", "X")
    )

  xnastring_obj <- XNAString(
    name = "b",
    base = Biostrings::DNAString(c("GGAG")),
    sugar = c("FFOO"),
    backbone = c("SSS"),
    dictionary = my_dic
  )

  out_base_1 <-
    XNAString::XNADinucleotideFrequency(
      obj = xnastring_obj,
      slot = "base",
      matrix_nbr = 1
    )
  out_base_2 <-
    XNAString::XNADinucleotideFrequency(
      obj = xnastring_obj,
      slot = "base",
      matrix_nbr = 2
    )
  out_base_3 <-
    XNAString::XNADinucleotideFrequency(
      obj = xnastring_obj,
      slot = "base",
      as.prob = TRUE
    )
  out_base_4 <-
    XNAString::XNADinucleotideFrequency(
      obj = xnastring_obj,
      slot = "base",
      double_letters = c("GE", "EG")
    )

  out_ref_base_1 <-
    matrix(
      c(0, 0, 1, 0, 0, 0, 1, 0, 1),
      nrow = 1,
      ncol = 9,
      byrow = TRUE
    )
  colnames(out_ref_base_1) <-
    c("AA", "EA", "GA", "AE", "EE", "GE", "AG", "EG", "GG")
  out_ref_base_1 <- formattable::formattable(out_ref_base_1, digits = 2, format = "f")

  out_ref_base_2 <-
    matrix(
      c(0, 0, 0, 0, 0, 0, 0, 0, 0),
      nrow = 1,
      ncol = 9,
      byrow = TRUE
    )
  colnames(out_ref_base_2) <-
    c("AA", "EA", "GA", "AE", "EE", "GE", "AG", "EG", "GG")
  out_ref_base_2 <- formattable::formattable(out_ref_base_2, digits = 2, format = "f")

  out_ref_base_3 <-
    matrix(
      c(0, 0, 0.3333333, 0, 0, 0, 0.3333333, 0, 0.3333333),
      nrow = 1,
      ncol = 9,
      byrow = TRUE
    )
  colnames(out_ref_base_3) <-
    c("AA", "EA", "GA", "AE", "EE", "GE", "AG", "EG", "GG")
  out_ref_base_3 <- formattable::formattable(out_ref_base_3, digits = 2, format = "f")

  out_ref_base_4 <-
    matrix(c(0, 0),
      nrow = 1,
      ncol = 2,
      byrow = TRUE
    )
  colnames(out_ref_base_4) <- c("GE", "EG")
  out_ref_base_4 <- formattable::formattable(out_ref_base_4, digits = 2, format = "f")

  testthat::expect_equal(out_base_1, out_ref_base_1)
  testthat::expect_equal(out_base_2, out_ref_base_2)
  testthat::expect_equal(out_base_3, out_ref_base_3, tolerance = 1e-3)
  testthat::expect_equal(out_base_4, out_ref_base_4)
})



testthat::test_that("dinucleotideFrequency gives corrrect results for XNAStringSet object", {
  my_dic <-
    data.table::data.table(
      type = c(rep("base", 3), rep("sugar", 2), rep("backbone", 3)),
      symbol = c("G", "E", "A", "F", "O", "S", "B", "X")
    )

  XNAString_obj1 <- XNAString(
    base = c("AGGE", "EEEA"),
    sugar = c("OOOO", "OOOO"),
    backbone = c("SBS", "SBS"),
    dictionary = my_dic
  )

  XNAString_obj2 <- XNAString(
    base = c("EGEA"),
    sugar = c("FFFO"),
    dictionary = my_dic
  )

  XNAStringSet_obj <- XNAStringSet(objects = list(
    XNAString_obj1,
    XNAString_obj2
  ))

  out_base_1 <-
    XNAString::XNADinucleotideFrequency(
      obj = XNAStringSet_obj,
      slot = "base",
      matrix_nbr = 1
    )
  out_base_2 <-
    XNAString::XNADinucleotideFrequency(
      obj = XNAStringSet_obj,
      slot = "base",
      matrix_nbr = 2
    )

  out_ref_base_1 <-
    matrix(
      c(0.00, 0.00, 0.00, 0.00, 0.00, 1.00, 1.00, 0.00, 1.00, 0.00, 1.00, 0.00, 0.00, 0.00, 1.00, 0.00, 1.00, 0.00),
      nrow = 2,
      ncol = 9,
      byrow = TRUE
    )
  colnames(out_ref_base_1) <-
    c("AA", "EA", "GA", "AE", "EE", "GE", "AG", "EG", "GG")
  out_ref_base_1 <- formattable::formattable(out_ref_base_1, digits = 2, format = "f")

  out_ref_base_2 <-
    matrix(
      c(0.00, 1.00, 0.00, 0.00, 2.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00),
      nrow = 2,
      ncol = 9,
      byrow = TRUE
    )
  colnames(out_ref_base_2) <-
    c("AA", "EA", "GA", "AE", "EE", "GE", "AG", "EG", "GG")
  out_ref_base_2 <- formattable::formattable(out_ref_base_2, digits = 2, format = "f")

  testthat::expect_equal(out_base_1, out_ref_base_1)
  testthat::expect_equal(out_base_2, out_ref_base_2)
})
