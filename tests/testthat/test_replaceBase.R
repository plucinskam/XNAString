
xnastring_obj <- XNAString(base = "ACGCAGTGACGAACA", sugar = "DMDMDDDDDDDDDDD")

testthat::test_that("Replacing dinucleotide works", {
  out <-  replaceBase(
        xnastring_obj,
        base_to_replace = "CG",
        base_replacement = "EG",
        sugar = "DD"
    )
  
  testthat::expect_equal("ACGCAGTGAEGAACA", out@base)
})


testthat::test_that("Replacing single nucleotide works", {
    out <- replaceBase(
        xnastring_obj,
        base_to_replace = "C",
        base_replacement = "E",
        sugar = "M"
    )
    
    testthat::expect_equal("AEGEAGTGACGAACA", out@base)
})
