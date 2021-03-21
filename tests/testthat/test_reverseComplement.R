context("reverseComplement")

testthat::test_that("Reverse complement works correctly (1)", {
  obj1 <- XNAString::XNAString(
    base = "ACAGETE",
    sugar = "FODDDDD",
    conjugate3 = "TAG"
  )

  revcom <- XNAReverseComplement(obj1)
  ref <- "GAGCTGT"

  testthat::expect_equal(revcom, ref)
})

testthat::test_that("Reverse complement works correctly (2)", {
  obj2 <- XNAString(base = Biostrings::DNAString("ACAGGTGGT"))
  revcom <- XNAReverseComplement(obj2)
  ref <- Biostrings::DNAString("ACCACCTGT")
  testthat::expect_equal(revcom, ref)
})


testthat::test_that("Reverse complement works correctly with custom dictionary", {
  compl_dict <-
    data.table::data.table(
      base = c("I", "J", "K"),
      target = c("A", "B", "C"),
      compl_target = c("I", "J", "K")
    )
  # J, K are not present in base dictionary - add them manually
  dict <-
    data.table::data.table(
      HELM = c(rep("", 5)),
      type = c(rep("base", 3), "sugar", "backbone"),
      symbol = c("I", "J", "K", "K", "K")
    )

  obj <- XNAString(
    base = "IJKK",
    sugar = "KKKK",
    backbone = "KKK",
    dictionary = dict,
    compl_dictionary = compl_dict
  )

  testthat::expect_equal(XNAReverseComplement(obj), c("CCCA", "CCGA", "CCTA"))
})

testthat::test_that("Reverse complement generates all combinations", {
  compl_dict <-
    data.table::data.table(
      base = c("I", "J", "K"),
      target = c("A", "B", "C"),
      compl_target = c("I", "J", "K")
    )
  dict <-
    data.table::data.table(
      HELM = c(rep("", 5)),
      type = c(rep("base", 3), "sugar", "backbone"),
      symbol = c("I", "J", "K", "K", "K")
    )

  obj <- XNAString(
    base = "IJKJ",
    sugar = "KKKK",
    backbone = "KKK",
    dictionary = dict,
    compl_dictionary = compl_dict
  )
  revcom <- XNAReverseComplement(obj)
  testthat::expect_equal(length(revcom), 9)
})
