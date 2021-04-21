testthat::test_that("Do origin wrapper function checks work", {

  # Push dummy R script to a temp directory --------------------------------------
  dir <- tempdir()
  test_file_path <- file.path(dir, "testfile.R")
  target_file_path <- file.path(dir, "targetfile.R")

  # works only when package is build
  datapath <- system.file("testdata", package = "origin")

  test_text <- read.csv2(file = file.path(datapath, "testscript.csv"),
                         na.strings = "NA",
                         encoding = "UTF-8",
                         stringsAsFactors = FALSE)

  # windows adds X.U.FEFF to the first variable name in read.csv2.
  # Therefore, the following bulletproof way to access the correct variable / vector
  nms <- names(test_text)
  writeLines(test_text[, grepl("TARGET", nms, fixed = TRUE)], con = target_file_path)
  writeLines(test_text[, grepl("TESTSKRIPT", nms, fixed = TRUE)], con = test_file_path)



  # add base packages but do not check for base function conflicts
  script <- test_text[, grepl("TESTSKRIPT", nms, fixed = TRUE)]
  testthat::expect_error(originize_wrap(scripts = list(script),
                                        files = test_file_path,
                                        type = "writeLines",
                                        add_base_packages = TRUE,
                                        check_base_conflicts = FALSE),
                         regexp = "When adding base packages checking for potential conflicts is required.",
                         fixed = TRUE)

  # ask user but disable verbose
  testthat::expect_error(originize_wrap(scripts = list(script),
                                        files = test_file_path,
                                        type = "writeLines",
                                        ask_before_applying_changes = TRUE,
                                        verbose = FALSE),
                         regexp = "Without verbose == TRUE no changes are visible before applying. `verbose` must be TRUE if ask_before_applying_changes is TRUE.",
                         fixed = TRUE)

  # zero length packages
  testthat::expect_error(originize_wrap(scripts = list(script),
                                        files = test_file_path,
                                        type = "writeLines",
                                        pkgs = NULL,
                                        check_base_conflicts = TRUE,
                                        verbose = FALSE,
                                        ask_before_applying_changes = FALSE),
                         regexp = "No packages specified. Please use either `options(origin.pkgs = c('pkg', ...))` or the `pkgs` argument.",
                         fixed = TRUE)

  # zero length packages
  testthat::expect_error(originize_wrap(scripts = list(script),
                                        files = test_file_path,
                                        type = "writeLines",
                                        pkgs = "utils",
                                        check_base_conflicts = FALSE,
                                        verbose = FALSE,
                                        ask_before_applying_changes = FALSE),
                         regexp = "No packages specified. Please use either `options(origin.pkgs = c('pkg', ...))` or the `pkgs` argument.",
                         fixed = TRUE)

  # do not check base functions but only use base functions
  testthat::expect_error(originize_wrap(scripts = list(script),
                                        files = test_file_path,
                                        type = "writeLines",
                                        pkgs = "base",
                                        check_base_conflicts = TRUE,
                                        verbose = FALSE,
                                        ask_before_applying_changes = FALSE),
                         regexp = paste("No packages specified. Please use either `options(origin.pkgs = c('pkg', ...))`",
                                        "or the `pkgs` argument. If you desire to use base",
                                        "packages, inspect the `add_base_packages` argument/option."),
                         fixed = TRUE)

  # packages do not export functions
  testthat::expect_error(originize_wrap(scripts = list(script),
                                        files = test_file_path,
                                        type = "writeLines",
                                        pkgs = "datasets",
                                        add_base_packages = TRUE,
                                        verbose = FALSE,
                                        ask_before_applying_changes = FALSE),
                         regexp = "Given packages do no export functions.",
                         fixed = TRUE)


  # exclude all functions from packages
  testthat::expect_error(originize_wrap(scripts = list(script),
                                        files = test_file_path,
                                        type = "writeLines",
                                        pkgs = "data.table",
                                        excluded_functions = list(data.table = get_exported_functions("data.table")),
                                        add_base_packages = FALSE,
                                        verbose = FALSE,
                                        ask_before_applying_changes = FALSE),
                         regexp = "You excluded all exported functions from the given packages.",
                         fixed = TRUE)

  # exclude all functions from packages
  testthat::expect_error(originize_wrap(scripts = list(script),
                                        files = test_file_path,
                                        type = "writeLines",
                                        pkgs = c("data.table", "utils"),
                                        excluded_functions = list(data.table = get_exported_functions("data.table")),
                                        add_base_packages = FALSE,
                                        verbose = FALSE,
                                        ask_before_applying_changes = FALSE),
                         regexp = "No non-excluded exported functions in given packages.",
                         fixed = TRUE)

  testthat::expect_message(originize_wrap(scripts = list(script),
                                        files = test_file_path,
                                        type = "writeLines",
                                        pkgs = "testthat",
                                        add_base_packages = FALSE,
                                        verbose = FALSE,
                                        ask_before_applying_changes = FALSE),
                         regexp = "No unspecified functions detected. Script remains as is.",
                         fixed = TRUE)



})
