


# Unit tests
testthat::test_that("origin file", {
  # Push dummy R script to a temp directory --------------------------------------
  dir <- tempdir()
  test_file_path <- file.path(dir, "testfile.R")
  target_file_path <- file.path(dir, "targetfile.R")

  # works only when package is build
  datapath <- system.file("testdata", package = "origin")

  test_text <- read.csv2(file = file.path(datapath, "testscript.csv"),
                         na.strings = "NA",
                         stringsAsFactors = FALSE)
  print("in testthat")
  print(paste("test_text:", class(test_text$TESTSKRIPT)))
  print( test_text$TESTSKRIPT)
  writeLines(test_text$TARGET, con = target_file_path)
  print("step1")
  writeLines(test_text$TESTSKRIPT, con = test_file_path)
  print("step2")
  script <- readLines(test_file_path)

  # In einem Schritt, mit crosschecks
  # capture.output(
  originize_file(test_file_path,
                 pkgs = c("data.table",
                          "dplyr",
                          # "testthat",
                          "purrr"
                 ),
                 overwrite = TRUE,
                 add_base_packages = FALSE,
                 ask_before_applying_changes = FALSE,
                 excluded_functions = list(dplyr = "last"),
                 ignore_comments = TRUE,
                 use_markers = FALSE,
                 verbose = FALSE)
  # )

  testfile_after <- readLines(test_file_path)

  testthat::expect_equal(testfile_after, test_text$TARGET)
})
