context("dictFromMimir")
testthat::test_that(
  desc = "checks if reformation of the mimir table works",
  code = {
    dt <- data.table::data.table(
      HELM = c(
        "([PPG])",
        "[fR]",
        "[srP]"
      ),
      TS_BASE_SEQ = c("F", NA, NA),
      TS_SUGAR_SEQ = c(NA, NA, "F"),
      TS_BACKBONE_SEQ = c(NA, "S", NA)
    )

    out <- mimir2XnaDict(
      dt,
      "TS_BASE_SEQ",
      "TS_SUGAR_SEQ",
      "TS_BACKBONE_SEQ"
    )

    out_ref <- data.table(
      HELM = c("([PPG])", "[srP]", "[fR]"),
      type = c("base", "sugar", "backbone"),
      symbol = c("F", "F", "S")
    )

    testthat::expect_equal(out, out_ref)
  }
)
