context("setterGetter")

testthat::test_that(
  desc = "checks if XNAString  setter/getter works",
  code = {
    my_dic <-
      data.table::data.table(
        type = c(
          rep("base", 3),
          rep("sugar", 2),
          rep("backbone", 3)
        ),
        symbol = c("G", "C", "A", "F", "O", "S", "B", "X")
      )

    obj <-
      XNAString(
        base = "GCGG",
        sugar = "FOOO",
        backbone = "SSS",
        target = Biostrings::DNAStringSet("CGCC"),
        dictionary = my_dic
      )
    testthat::expect_equal(base(obj), "GCGG")
    base(obj) <- "CCCC"
    testthat::expect_equal(base(obj), "CCCC")
  }
)


testthat::test_that(
  desc = "checks if XNAString  setter/getter works when base no character",
  code = {
    my_dic <-
      data.table::data.table(
        type = c(
          rep("base", 3),
          rep("sugar", 2),
          rep("backbone", 3)
        ),
        symbol = c("G", "C", "A", "F", "O", "S", "B", "X")
      )

    obj <-
      XNAString(
        base = Biostrings::DNAString("GCGG"),
        sugar = "FOOO",
        backbone = "SSS",
        target = Biostrings::DNAStringSet("CGCC"),
        dictionary = my_dic
      )
    testthat::expect_equal(base(obj), "GCGG")
    base(obj) <- "CCCC"
    testthat::expect_equal(base(obj), "CCCC")
  }
)


testthat::test_that(
  desc = "checks if XNAStringSet slots setter and getter works when base is non character",
  code = {
    my_dic <-
      data.table::data.table(
        type = c(
          rep("base", 3),
          rep("sugar", 2),
          rep("backbone", 3)
        ),
        symbol = c("C", "G", "A", "F", "O", "S", "O", "X")
      )

    obj1 <-
      XNAString(
        name = "a",
        base = Biostrings::DNAStringSet(c("CCC", "GCG")),
        sugar = c("FFO", "OOO"),
        dictionary = my_dic
      )
    obj2 <-
      XNAString(
        name = "b",
        base = Biostrings::DNAStringSet(c("GGG", "CCC")),
        sugar = c("OOO", "OOO"),
        dictionary = my_dic
      )
    XNAStringSetObj <- XNAStringSet(objects = list(obj1, obj2))

    testthat::expect_equal(
      base(XNAStringSetObj),
      c("CCC", "GGG")
    )
    testthat::expect_equal(
      base(XNAStringSetObj, 2),
      c("GCG", "CCC")
    )

    base(XNAStringSetObj, 2) <- c("AAA", "AAA")
    testthat::expect_equal(
      base(XNAStringSetObj, 2),
      c("AAA", "AAA")
    )
  }
)

testthat::test_that(
  desc = "checks if XNAStringSet slots setter and getter works",
  code = {
    my_dic <-
      data.table::data.table(
        type = c(
          rep("base", 3),
          rep("sugar", 2),
          rep("backbone", 3)
        ),
        symbol = c("E", "G", "A", "F", "O", "S", "B", "X")
      )

    obj1 <-
      XNAString(
        name = "a",
        base = c("EEE", "GEG"),
        sugar = c("FFO", "OOO"),
        dictionary = my_dic
      )
    obj2 <-
      XNAString(
        name = "b",
        base = c("GGG", "EEE"),
        sugar = c("OOO", "OOO"),
        dictionary = my_dic
      )
    XNAStringSetObj <- XNAStringSet(objects = list(obj1, obj2))

    testthat::expect_equal(
      base(XNAStringSetObj),
      c("EEE", "GGG")
    )
    testthat::expect_equal(
      base(XNAStringSetObj, 2),
      c("GEG", "EEE")
    )

    base(XNAStringSetObj, 2) <- c("AAA", "AAA")
    testthat::expect_equal(
      base(XNAStringSetObj, 2),
      c("AAA", "AAA")
    )
  }
)

testthat::test_that(
  desc = "checks if XNAStringSet slots setter and getter works (2)",
  code = {
    my_dic <-
      data.table::data.table(
        type = c(
          rep("base", 3),
          rep("sugar", 2),
          rep("backbone", 3)
        ),
        symbol = c("E", "G", "A", "F", "O", "S", "B", "X")
      )

    obj1 <-
      XNAString(
        name = "a",
        base = c("EEE"),
        sugar = c("FFO"),
        dictionary = my_dic
      )
    obj2 <-
      XNAString(
        name = "b",
        base = c("GGG", "EEE"),
        sugar = c("OOO", "FFF"),
        dictionary = my_dic
      )
    XNAStringSetObj <- XNAStringSet(objects = list(obj1, obj2))

    testthat::expect_equal(
      base(XNAStringSetObj),
      c("EEE", "GGG")
    )
    testthat::expect_equal(
      base(XNAStringSetObj, 2),
      c("", "EEE")
    )

    base(XNAStringSetObj, 1) <- c("AAA", "AAA")
    testthat::expect_equal(
      base(XNAStringSetObj, 1),
      c("AAA", "AAA")
    )
  }
)


testthat::test_that(
  desc = "checks if XNAStringSet slots setter and getter works when bas non character (2)",
  code = {
    my_dic <-
      data.table::data.table(
        type = c(
          rep("base", 3),
          rep("sugar", 2),
          rep("backbone", 3)
        ),
        symbol = c("C", "G", "A", "F", "O", "S", "O", "X")
      )

    obj1 <-
      XNAString(
        name = "a",
        base = Biostrings::DNAString("CCC"),
        sugar = c("FFO"),
        dictionary = my_dic
      )
    obj2 <-
      XNAString(
        name = "b",
        base = c("GGG", "CCC"),
        sugar = c("OOO", "FFF"),
        dictionary = my_dic
      )
    XNAStringSetObj <- XNAStringSet(objects = list(obj1, obj2))

    testthat::expect_equal(
      base(XNAStringSetObj),
      c("CCC", "GGG")
    )
    testthat::expect_equal(
      base(XNAStringSetObj, 2),
      c("", "CCC")
    )

    base(XNAStringSetObj, 1) <- c("AAA", "AAA")
    testthat::expect_equal(
      base(XNAStringSetObj, 1),
      c("AAA", "AAA")
    )
  }
)
