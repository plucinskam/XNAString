context("XNAString2Helm")

testthat::test_that("XNAString2Helm gives correct result for double stranded molecule", {
  obj <- XNAString(
    base = c("CCCCEGC", "UUAUGAT"),
    sugar = c("OOFOFOF", "FFOFOFO"),
    backbone = c("OOOOOO", "OOOOOO"),
    target = '',
    conjugate3 = "",
    conjugate5 = "")
  
  res <- XNAStringToHelm(obj)
  ref <- "RNA1{[mR](C)P.[mR](C)P.[fR](C)P.[mR](C)P.[fR]([5meC])P.[mR](G)P.[fR](C)}|RNA2{[fR](U)P.[fR](U)P.[mR](A)P.[fR](U)P.[mR](G)P.[fR](A)P.[mR](T)}$RNA1,RNA2,2:pair-20:pair|RNA1,RNA2,5:pair-17:pair|RNA1,RNA2,8:pair-14:pair|RNA1,RNA2,11:pair-11:pair|RNA1,RNA2,14:pair-8:pair|RNA1,RNA2,17:pair-5:pair|RNA1,RNA2,20:pair-2:pair$$$V2.0"
  
  testthat::expect_equal(res, ref)
})


testthat::test_that("XNAString2Helm gives correct result for single stranded molecule (1)", {
  obj <- XNAString(base = 'AAA',
                   sugar = 'DDD',
                   backbone = 'OO')
  
  res <- XNAStringToHelm(obj)
  ref <- "RNA1{[dR](A)P.[dR](A)P.[dR](A)}$$$V2.0"
  
  testthat::expect_equal(res, ref)
})


testthat::test_that("XNAString2Helm gives correct result for single stranded molecule (2)", {
  obj <- XNAString(base = 'GAGTTACTTGCCAAET',
                   sugar = 'LLLDMDDDDDDDDLLL',
                   backbone = 'XXXXXXXXXXXXXX2')
  
  res <- XNAStringToHelm(obj)
  ref <- "RNA1{[LR](G)[sP].[LR](A)[sP].[LR](G)[sP].[dR](T)[sP].[MOE](T)[sP].[dR](A)[sP].[dR](C)[sP].[dR](T)[sP].[dR](T)[sP].[dR](G)[sP].[dR](C)[sP].[dR](C)[sP].[dR](A)[sP].[LR](A)[sP].[LR]([5meC])[PS2].[LR](T)}$$$V2.0"
  
  testthat::expect_equal(res, ref)
})

testthat::test_that("XNAString2Helm gives correct result for single stranded molecule with base non character", {
  obj <- XNAString(base = Biostrings::DNAString('GAGTTACTTGCCAAAT'),
                   sugar = 'LLLDMDDDDDDDDLLL',
                   backbone = 'XXXXXXXXXXXXXX2')
  
  res <- XNAStringToHelm(obj)
  ref <- "RNA1{[LR](G)[sP].[LR](A)[sP].[LR](G)[sP].[dR](T)[sP].[MOE](T)[sP].[dR](A)[sP].[dR](C)[sP].[dR](T)[sP].[dR](T)[sP].[dR](G)[sP].[dR](C)[sP].[dR](C)[sP].[dR](A)[sP].[LR](A)[sP].[LR](A)[PS2].[LR](T)}$$$V2.0"
  
  testthat::expect_equal(res, ref)
})


testthat::test_that("XNAString2Helm gives correct result if base is DNAString", {
  obj <- XNAString(
    base = Biostrings::DNAStringSet(c("CCCC", "GGGG")),
    sugar = c("OOFO", "FFOF"),
    backbone = c("OOO", "OOO"),
    target = '',
    conjugate3 = "",
    conjugate5 = "")
  
  res <- XNAStringToHelm(obj)
  ref <- "RNA1{[mR](C)P.[mR](C)P.[fR](C)P.[mR](C)}|RNA2{[fR](G)P.[fR](G)P.[mR](G)P.[fR](G)}$RNA1,RNA2,2:pair-11:pair|RNA1,RNA2,5:pair-8:pair|RNA1,RNA2,8:pair-5:pair|RNA1,RNA2,11:pair-2:pair$$$V2.0"
  
  testthat::expect_equal(res, ref)
})




testthat::test_that("siRNA_HELM returns pairing information correctly", {
  
  pairing_ref <- "RNA1,RNA2,2:pair-56:pair|RNA1,RNA2,5:pair-53:pair|RNA1,RNA2,8:pair-50:pair|RNA1,RNA2,11:pair-47:pair|RNA1,RNA2,14:pair-44:pair|RNA1,RNA2,17:pair-41:pair|RNA1,RNA2,20:pair-38:pair|RNA1,RNA2,23:pair-35:pair|RNA1,RNA2,26:pair-32:pair|RNA1,RNA2,29:pair-29:pair|RNA1,RNA2,32:pair-26:pair|RNA1,RNA2,35:pair-23:pair|RNA1,RNA2,38:pair-20:pair|RNA1,RNA2,41:pair-17:pair|RNA1,RNA2,44:pair-14:pair|RNA1,RNA2,47:pair-11:pair|RNA1,RNA2,50:pair-8:pair|RNA1,RNA2,53:pair-5:pair|RNA1,RNA2,56:pair-2:pair"
  
  obj1 <- XNAString(base =  c("CCCCUGCCGUGGUUCAUAA", "UUAUGAACCACGGCAGGGGCG"), 
                    sugar = c("OOFOFOFOFOFOFOFOFOF", "FFOFOFOFOFOFOFOFOFOFO"),
                    backbone = c("OOOOOOOOOOOOOOOOOO", "OOOOOOOOOOOOOOOOOOOO"),
                    conjugate3 = c(''))
  
  pairing_res <- siRNA_HELM(obj1)
  
  testthat::expect_equal(pairing_res, pairing_ref)
})




testthat::test_that("XNAStringToHelm gives additionaly pairing info for double stranded molecule", {
  
  obj1 <- XNAString(base =  c("CCCCUGCCGUGGUUCAUAA", "UUAUGAACCACGGCAGGGGCG"), 
                    sugar = c("OOFOFOFOFOFOFOFOFOF", "FFOFOFOFOFOFOFOFOFOFO"),
                    backbone = c("OOOOOOOOOOOOOOOOOO", "OOOOOOOOOOOOOOOOOOOO"),
                    conjugate3 = c(''))
  
  ref <-
    "RNA1{[mR](C)P.[mR](C)P.[fR](C)P.[mR](C)P.[fR](U)P.[mR](G)P.[fR](C)P.[mR](C)P.[fR](G)P.[mR](U)P.[fR](G)P.[mR](G)P.[fR](U)P.[mR](U)P.[fR](C)P.[mR](A)P.[fR](U)P.[mR](A)P.[fR](A)}|RNA2{[fR](U)P.[fR](U)P.[mR](A)P.[fR](U)P.[mR](G)P.[fR](A)P.[mR](A)P.[fR](C)P.[mR](C)P.[fR](A)P.[mR](C)P.[fR](G)P.[mR](G)P.[fR](C)P.[mR](A)P.[fR](G)P.[mR](G)P.[fR](G)P.[mR](G)P.[fR](C)P.[mR](G)}$RNA1,RNA2,2:pair-56:pair|RNA1,RNA2,5:pair-53:pair|RNA1,RNA2,8:pair-50:pair|RNA1,RNA2,11:pair-47:pair|RNA1,RNA2,14:pair-44:pair|RNA1,RNA2,17:pair-41:pair|RNA1,RNA2,20:pair-38:pair|RNA1,RNA2,23:pair-35:pair|RNA1,RNA2,26:pair-32:pair|RNA1,RNA2,29:pair-29:pair|RNA1,RNA2,32:pair-26:pair|RNA1,RNA2,35:pair-23:pair|RNA1,RNA2,38:pair-20:pair|RNA1,RNA2,41:pair-17:pair|RNA1,RNA2,44:pair-14:pair|RNA1,RNA2,47:pair-11:pair|RNA1,RNA2,50:pair-8:pair|RNA1,RNA2,53:pair-5:pair|RNA1,RNA2,56:pair-2:pair$$$V2.0"
  
  
  res <- XNAStringToHelm(obj1) 
  
  testthat::expect_equal(res, ref)
})
