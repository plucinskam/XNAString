context("helm2String")

testthat::test_that("hlm2String gives corrrect results #1", {
  helm <-
    "RNA1{[LR](G)[sP].[LR](A)[sP].[LR](G)[sP].[dR](T)[sP].[MOE](T)[sP].[dR](A)[sP].[dR](C)[sP].[dR](T)[sP].[dR](T)[sP].[dR](G)[sP].[dR](C)[sP].[dR](C)[sP].[dR](A)[sP].[LR](A)[sP].[LR]([5meC])[PS2].[LR](T)}$$$$V2.0"
  
  out_ref <- list(
    base = "GAGTTACTTGCCAAET",
    sugar = "LLLDMDDDDDDDDLLL",
    backbone = "XXXXXXXXXXXXXX2",
    conjugate3 = "",
    conjugate5 = ""
  )
  
  out <- XNAString::helm2String(helm)
  
  testthat::expect_equal(out, out_ref)
})

testthat::test_that("hlm2String gives corrrect results #2", {
  helm <-
    "RNA1{[LR](T)[sP].[LR](G)[sP].[dR](T)[sP].[dR](G)[sP].[LR](T)[sP].[LR](G)[sP].[dR](T)[sP].[dR](G)[sP].[LR](T)[sP].[LR](G)[sP].[dR](T)[sP].[dR](G)[sP].[LR](T)[sP].[LR](G)[sP].[LR](T)}$$$$V2.0"
  
  out_ref <- list(
    base = "TGTGTGTGTGTGTGT",
    sugar = "LLDDLLDDLLDDLLL",
    backbone = "XXXXXXXXXXXXXX",
    conjugate3 = "",
    conjugate5 = ""
  )
  
  out <- XNAString::helm2String(helm)
  
  testthat::expect_equal(out, out_ref)
})

testthat::test_that("hlm2String handles elements not found in dictonary", {
  helm <-
    "RNA1{[LR](K)[sP].[LR](G)}$$$$V2.0"
  
  testthat::expect_error(XNAString::helm2String(helm),
                         "Base (K) is not present in dictionary.",
                         fixed = TRUE)
})


testthat::test_that("parseRnaHelmComponent works correctly", {
  helm <- c("[LR](C)[sP]", "[LR](G)")
  
  testthat::expect_equal(
    XNAString:::parseRnaHelmComponent(helm),
    list(
      base = "CG",
      sugar = "LL",
      backbone = "X"
    )
  )
})

testthat::test_that("hlm2String removes linkers correctly", {
  helm <-
    "CHEM1{[5gn2c6]}|RNA1{P.[dR](C)P.[dR](A)P.[LR](G)[sP].[LR](A)[sP].[LR](G)[sP].[LR](A)[sP].[dR](A)[sP].[dR](G)[sP].[dR](G)[sP].[dR](C)[sP].[dR](A)[sP].[dR](C)[sP].[dR](A)[sP].[dR](G)[sP].[dR](A)[sP].[LR]([5meC])[sP].[LR](G)[sP].[LR](G)}$CHEM1,RNA1,1:R2-1:R1$$$V2.0"
  
  out_ref <- list(
    base = "GAGAAGGCACAGAEGG",
    sugar = "LLLLDDDDDDDDDLLL",
    backbone = "XXXXXXXXXXXXXXX",
    conjugate3 = "",
    conjugate5 = "[5gn2c6]"
  )
  
  out <- XNAString::helm2String(helm)
  
  testthat::expect_equal(out, out_ref)
})

testthat::test_that("hlm2String keeps linker if remove_linker == FALSE", {
  helm <-
    "CHEM1{[5gn2c6]}|RNA1{P.[dR](C)P.[dR](A)P.[LR](G)[sP].[LR](A)[sP].[LR](G)[sP].[LR](A)[sP].[dR](A)[sP].[dR](G)[sP].[dR](G)[sP].[dR](C)[sP].[dR](A)[sP].[dR](C)[sP].[dR](A)[sP].[dR](G)[sP].[dR](A)[sP].[LR]([5meC])[sP].[LR](G)[sP].[LR](G)}$CHEM1,RNA1,1:R2-1:R1$$$V2.0"
  
  out_ref <- list(
    base = "CAGAGAAGGCACAGAEGG",
    sugar = "DDLLLLDDDDDDDDDLLL",
    backbone = "OOXXXXXXXXXXXXXXX",
    conjugate3 = "",
    conjugate5 = "[5gn2c6]"
  )
  
  out <- XNAString::helm2String(helm = helm, remove_linker = FALSE)
  
  testthat::expect_equal(out, out_ref)
})


testthat::test_that("helm2String gives corrrect results for multiple RNA's", {
  dict <- XNAString::xna_dictionary
  
  helm <-
    "RNA1{[mR](C)P.[mR](C)P.[fR](C)P.[mR](C)P.[fR](U)P.[mR](G)P.[fR](C)P.[mR](C)P.[fR](G)P.[mR](U)P.[fR](G)P.[mR](G)P.[fR](U)P.[mR](U)P.[fR](C)P.[mR](A)P.[fR](U)P.[mR](A)P.[fR](A)}|RNA2{[fR](U)P.[fR](U)P.[mR](A)P.[fR](U)P.[mR](G)P.[fR](A)P.[mR](A)P.[fR](C)P.[mR](C)P.[fR](A)P.[mR](C)P.[fR](G)P.[mR](G)P.[fR](C)P.[mR](A)P.[fR](G)P.[mR](G)P.[fR](G)P.[mR](G)P.[fR](C)P.[mR](G)}$RNA1,RNA2,2:pair-56:pair|RNA1,RNA2,5:pair-53:pair|RNA1,RNA2,8:pair-50:pair|RNA1,RNA2,11:pair-47:pair|RNA1,RNA2,14:pair-44:pair|RNA1,RNA2,17:pair-41:pair|RNA1,RNA2,20:pair-38:pair|RNA1,RNA2,23:pair-35:pair|RNA1,RNA2,26:pair-32:pair|RNA1,RNA2,29:pair-29:pair|RNA1,RNA2,32:pair-26:pair|RNA1,RNA2,35:pair-23:pair|RNA1,RNA2,38:pair-20:pair|RNA1,RNA2,41:pair-17:pair|RNA1,RNA2,44:pair-14:pair|RNA1,RNA2,47:pair-11:pair|RNA1,RNA2,50:pair-8:pair|RNA1,RNA2,53:pair-5:pair|RNA1,RNA2,56:pair-2:pair$$$V2.0"
  
  out_ref <- list(
    base = c("CCCCUGCCGUGGUUCAUAA", "UUAUGAACCACGGCAGGGGCG"),
    sugar = c("OOFOFOFOFOFOFOFOFOF", "FFOFOFOFOFOFOFOFOFOFO"),
    backbone = c("OOOOOOOOOOOOOOOOOO", "OOOOOOOOOOOOOOOOOOOO"),
    conjugate3 = "",
    conjugate5 = ""
  )
  
  out <- XNAString::helm2String(helm, dict)
  
  testthat::expect_equal(out, out_ref)
})


