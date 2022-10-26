testthat::test_that("Do origin wrapper function checks work", {
  
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
  
  
  
  # add base packages but do not check for base function conflicts
  script <- test_text[, grepl("TESTSKRIPT", nms, fixed = TRUE)]
  testthat::expect_error(originize_wrap(scripts = list(script),
                                        files = test_file_path,
                                        type = "writeLines",
                                        add_base_packages = TRUE,
                                        check_local_conflicts = FALSE,
                                        check_base_conflicts = FALSE),
                         regexp = paste("When adding base packages checking",
                                        "for potential conflicts is required."),
                         fixed = TRUE)
  
  # ask user but disable verbose
  testthat::expect_error(
    originize_wrap(scripts = list(script),
                   files = test_file_path,
                   type = "writeLines",
                   ask_before_applying_changes = TRUE,
                   check_local_conflicts = FALSE,
                   verbose = FALSE),
    regexp = paste("Without verbose == TRUE no changes are visible before",
                   "applying. `verbose` must be TRUE if",
                   "ask_before_applying_changes is TRUE."),
    fixed = TRUE)
  
  # zero length packages
  testthat::expect_error(
    originize_wrap(scripts = list(script),
                   files = test_file_path,
                   type = "writeLines",
                   pkgs = NULL,
                   check_base_conflicts = TRUE,
                   verbose = FALSE,
                   check_local_conflicts = FALSE,
                   ask_before_applying_changes = FALSE),
    regexp = paste("No packages specified. Please use either",
                   "`options(origin.pkgs = c('pkg', ...))`",
                   "or the `pkgs` argument."),
    fixed = TRUE)
  
  # zero length packages
  testthat::expect_error(
    originize_wrap(scripts = list(script),
                   files = test_file_path,
                   type = "writeLines",
                   pkgs = "utils",
                   check_base_conflicts = FALSE,
                   verbose = FALSE,
                   check_local_conflicts = FALSE,
                   ask_before_applying_changes = FALSE),
    regexp = paste("No packages specified. Please use either",
                   "`options(origin.pkgs = c('pkg', ...))`",
                   "or the `pkgs` argument."), # Exclude Linting
    fixed = TRUE)
  
  # do not check base functions but only use base functions
  testthat::expect_error(
    originize_wrap(scripts = list(script),
                   files = test_file_path,
                   type = "writeLines",
                   pkgs = "base",
                   check_base_conflicts = TRUE,
                   check_local_conflicts = FALSE,
                   add_base_packages = FALSE,
                   verbose = FALSE,
                   ask_before_applying_changes = FALSE),
    regexp = paste("No packages specified. Please use either",
                   "`options(origin.pkgs = c('pkg', ...))`",
                   "or the `pkgs` argument. If you desire to use base",
                   "packages, inspect the `add_base_packages` argument/option."), # Exclude Linting
    fixed = TRUE)
  
  # packages do not export functions
  testthat::expect_error(originize_wrap(scripts = list(script),
                                        files = test_file_path,
                                        type = "writeLines",
                                        pkgs = "datasets",
                                        add_base_packages = TRUE,
                                        check_local_conflicts = FALSE,
                                        verbose = FALSE,
                                        ask_before_applying_changes = FALSE),
                         regexp = "Given packages do not export functions.",
                         fixed = TRUE)
  
  
  # exclude all functions from packages
  testthat::expect_error(
    originize_wrap(scripts = list(script),
                   files = test_file_path,
                   type = "writeLines",
                   pkgs = "data.table",
                   excluded_functions =
                     list(data.table = get_exported_functions("data.table")),
                   add_base_packages = FALSE,
                   verbose = FALSE,
                   check_local_conflicts = FALSE,
                   ask_before_applying_changes = FALSE),
    regexp = "You excluded all exported functions from the given packages.",
    fixed = TRUE)
  
  # exclude all functions from packages
  testthat::expect_error(
    originize_wrap(scripts = list(script),
                   files = test_file_path,
                   type = "writeLines",
                   pkgs = c("data.table", "utils"),
                   excluded_functions =
                     list(data.table = get_exported_functions("data.table")),
                   add_base_packages = FALSE,
                   check_local_conflicts = FALSE,
                   verbose = FALSE,
                   ask_before_applying_changes = FALSE),
    regexp = "No non-excluded exported functions in given packages.",
    fixed = TRUE)
  
  testthat::expect_message(originize_wrap(scripts = list(script),
                                          files = test_file_path,
                                          type = "writeLines",
                                          pkgs = "testthat",
                                          add_base_packages = FALSE,
                                          check_local_conflicts = FALSE,
                                          verbose = FALSE,
                                          ask_before_applying_changes = FALSE),
                           regexp = "Nothing detected",
                           fixed = TRUE)
  testthat::expect_message(originize_wrap(scripts = list(script),
                                          files = test_file_path,
                                          type = "writeLines",
                                          pkgs = "testthat",
                                          check_conflicts = FALSE,
                                          add_base_packages = FALSE,
                                          check_local_conflicts = FALSE,
                                          verbose = FALSE,
                                          ask_before_applying_changes = FALSE),
                           regexp = "Nothing detected",
                           fixed = TRUE)
  
  # Package is provided multiple times
  testthat::expect_message(
    testthat::expect_warning(
      originize_wrap(scripts = list(script),
                     files = test_file_path,
                     type = "writeLines",
                     pkgs = c("testthat", "testthat"),
                     add_base_packages = FALSE,
                     check_local_conflicts = FALSE,
                     verbose = FALSE,
                     ask_before_applying_changes = FALSE),
      regexp = "packages are provided more than once",
      fixed = TRUE),
    regexp = "Nothing detected",
    fixed = TRUE)
  
  # InsertText without hits
  testthat::expect_message(
    testthat::expect_equal(
      originize_wrap(scripts = list(script),
                     files = test_file_path,
                     type = "insertText",
                     pkgs = "testthat",
                     check_conflicts = TRUE,
                     add_base_packages = FALSE,
                     check_local_conflicts = FALSE,
                     verbose = FALSE,
                     ask_before_applying_changes = FALSE),
      NULL),
    regexp = "Nothing detected",
    fixed = TRUE)
  
  # Insert Text runs -----------------------------------------------------------
  testthat::expect_equal(
    originize_wrap(scripts = list(script),
                   files = test_file_path,
                   type = "insertText",
                   pkgs = "dplyr",
                   check_conflicts = FALSE,
                   add_base_packages = FALSE,
                   check_local_conflicts = FALSE,
                   verbose = FALSE,
                   ask_before_applying_changes = FALSE),
    NULL)
  
  # with logging
  if (!rstudioapi::isAvailable()) {
    testthat::expect_error(
      testthat::expect_equal(
        originize_wrap(scripts = list(script),
                       files = test_file_path,
                       type = "insertText",
                       pkgs = c("dplyr", "purrr"),
                       check_conflicts = FALSE,
                       add_base_packages = FALSE,
                       check_local_conflicts = FALSE,
                       verbose = TRUE,
                       ask_before_applying_changes = FALSE),
        NULL),
      regexp = "RStudio not running",
      fixed = TRUE)
  } else {
    testthat::expect_equal(
      originize_wrap(scripts = list(script),
                     files = test_file_path,
                     type = "insertText",
                     pkgs = c("dplyr", "purrr"),
                     check_conflicts = FALSE,
                     add_base_packages = FALSE,
                     check_local_conflicts = FALSE,
                     verbose = TRUE,
                     ask_before_applying_changes = FALSE),
      NULL)
    
  }
  
  
  # Local Conflicts are checked
  testthat::expect_message(
    if (rstudioapi::isAvailable()) {
      testthat::expect_warning(
        originize_wrap(scripts = list(script),
                       files = test_file_path,
                       type = "writeLines",
                       pkgs = "testthat",
                       add_base_packages = FALSE,
                       check_local_conflicts = TRUE,
                       verbose = FALSE,
                       ask_before_applying_changes = FALSE),
        regexp = "Cannot check for local functions",
        fixed = TRUE)
    } else {
      testthat::expect_warning(
        originize_wrap(scripts = list(script),
                       files = test_file_path,
                       type = "writeLines",
                       pkgs = "testthat",
                       add_base_packages = FALSE,
                       check_local_conflicts = TRUE,
                       verbose = FALSE,
                       ask_before_applying_changes = FALSE),
        regexp = "RStudio not running",
        fixed = TRUE)
    },
    regexp = "Nothing detected",
    fixed = TRUE)
  
  # no exported functions in script
  testthat::expect_message(
    originize_wrap(scripts = list(script[1:20]),
                   files = test_file_path,
                   type = "writeLines",
                   pkgs = "rstudioapi",
                   add_base_packages = FALSE,
                   check_local_conflicts = FALSE,
                   verbose = FALSE,
                   ask_before_applying_changes = FALSE),
    regexp = "Nothing detected",
    fixed = TRUE)
  
  # path to local functions doesnt exist
  testthat::expect_error(
    originize_wrap(path_to_local_functions = "foobar"),
    regexp = "does not exist",
    fixed = TRUE)
  
  
  
})
