


# Unit tests
testthat::test_that("originize file", {
  # Push dummy R script to a temp directory ------------------------------------
  dir <- tempdir()
  test_file_path <- file.path(dir, "testfile.R")
  target_file_path <- file.path(dir, "targetfile.R")

  # works only when package is build
  datapath <- system.file("testdata", package = "origin")

  test_text <- utils::read.csv2(file = file.path(datapath, "testscript.csv"),
                                na.strings = "NA",
                                encoding = "UTF-8",
                                stringsAsFactors = FALSE)

  # windows adds X.U.FEFF to the first variable name in read.csv2.
  # Therefore, the following bulletproof way to access
  # the correct variable / vector
  nms <- names(test_text)
  writeLines(test_text[, grepl("TARGET", nms, fixed = TRUE)],
             con = target_file_path)
  writeLines(test_text[, grepl("TESTSKRIPT", nms, fixed = TRUE)],
             con = test_file_path)

  # In einem Schritt, mit crosschecks
  originize_file(test_file_path,
                 pkgs = c("data.table",
                          "dplyr",
                          # "testthat",
                          "purrr"
                 ),
                 overwrite = TRUE,
                 add_base_packages = FALSE,
                 ask_before_applying_changes = FALSE,
                 use_markers = FALSE,
                 check_local_conflicts = FALSE,
                 verbose = FALSE)

  testfile_after <- readLines(test_file_path)

  testthat::expect_equal(testfile_after,
                         test_text[, grepl("TARGET", nms, fixed = TRUE)])

  # with verbose
  # reset data
  writeLines(test_text[, grepl("TESTSKRIPT", nms, fixed = TRUE)],
             con = test_file_path)

  # In einem Schritt, mit crosschecks
  utils::capture.output(
    originize_file(test_file_path,
                   pkgs = c("data.table",
                            "dplyr",
                            # "testthat",
                            "purrr"
                   ),
                   overwrite = TRUE,
                   add_base_packages = FALSE,
                   ask_before_applying_changes = FALSE,
                   check_local_conflicts = FALSE,
                   excluded_functions = list(dplyr = "last"),
                   use_markers = FALSE,
                   verbose = TRUE)
  )

  testfile_after <- readLines(test_file_path)

  testthat::expect_equal(testfile_after,
                         test_text[, grepl("TARGET", nms, fixed = TRUE)])


  # with Markers works only when called from within RStudio
  # reset data
  writeLines(test_text[, grepl("TESTSKRIPT", nms, fixed = TRUE)],
             con = test_file_path)

  if (rstudioapi::isAvailable()) {
    # In einem Schritt, mit crosschecks
      originize_file(test_file_path,
                     pkgs = c("data.table",
                              "dplyr",
                              # "testthat",
                              "purrr"
                     ),
                     overwrite = TRUE,
                     add_base_packages = FALSE,
                     check_local_conflicts = FALSE,
                     ask_before_applying_changes = FALSE,
                     excluded_functions = list(dplyr = "last"),
                     use_markers = TRUE,
                     verbose = TRUE)

    testfile_after <- readLines(test_file_path)

    testthat::expect_equal(testfile_after,
                           test_text[, grepl("TARGET", nms, fixed = TRUE)])
  } else {
    testthat::expect_error(
      originize_file(test_file_path,
                     pkgs = c("data.table",
                              "dplyr",
                              # "testthat",
                              "purrr"
                     ),
                     overwrite = TRUE,
                     add_base_packages = FALSE,
                     check_local_conflicts = FALSE,
                     ask_before_applying_changes = FALSE,
                     excluded_functions = list(dplyr = "last"),
                     use_markers = TRUE,
                     verbose = TRUE),
      regexp = "RStudio not running")
  }

  testthat::expect_error(
    originize_file(file = file.path("non", "existing", "file.R")),
    regexp = "No file in this path")


})
