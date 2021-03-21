### =========================================================================
### instanceOf function
### -------------------------------------------------------------------------
testthat::test_that(
  desc = "checks instanceOf function",
  code = {
    testthat::expect_that(instanceOf(1, "numeric"), equals(TRUE))
  }
)


### =========================================================================
### unique_chars function
### -------------------------------------------------------------------------
testthat::test_that(
  desc = "checks uniqueChars function",
  code = {
    # expect an error for numeric input
    testthat::expect_error(uniqueChars(124))
    # return vector of unique chars
    testthat::expect_that(
      uniqueChars("TRGFFTR")[[1]],
      equals(c("T", "R", "G", "F"))
    )
  }
)


### =========================================================================
### listOflists2Dt function
### -------------------------------------------------------------------------

testthat::test_that(
  desc = "checks if listOflists2Dt function works",
  code = {
    nested_list <- list(
      list(base = c("T"), sugar = c("G")),
      list(base = c("U"), sugar = c("G"))
    )
    dt <- listOflists2Dt(nested_list)

    testthat::expect_that(dt$base, equals(c("T", "U")))
  }
)




testthat::test_that(
  desc = "checks if changeBase function works",
  code = {
    res <- changeBase(complementary_bases, c("ACT", "EEG"))
    ref <- c("ACT", "CCG")

    testthat::expect_equal(res, ref)
  }
)
