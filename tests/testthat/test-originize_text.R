testthat::test_that("Highlighting of plain text", {

  # works only when package is build
  datapath <- system.file("testdata", package = "origin")

  test_text <- read.csv2(file = file.path(datapath, "testscript.csv"),
                         na.strings = "NA",
                         encoding = "UTF-8",
                         stringsAsFactors = FALSE)

  # windows adds X.U.FEFF to the first variable name in read.csv2.
  # Therefore, the following bulletproof way to access the correct variable / vector
  nms <- names(test_text)
  script_in <- test_text[, grepl("TESTSKRIPT", nms, fixed = TRUE)]
  script_out <- test_text[, grepl("TARGET", nms, fixed = TRUE)]

  result <- originize_text(text = script_in,
                           pkgs = c("data.table",
                                    "dplyr",
                                    "purrr"
                           ),
                           overwrite = TRUE,
                           check_conflicts = FALSE,
                           add_base_packages = FALSE,
                           ask_before_applying_changes = FALSE,
                           excluded_functions = list(dplyr = "last"),
                           ignore_comments = TRUE,
                           use_markers = FALSE,
                           verbose = FALSE)

  testthat::expect_equal(result, paste(script_out, collapse = "\n"))

})
