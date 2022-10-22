testthat::test_that("check_pkg_usage() working", {
  dir <- tempfile()
  dir.create(dir)
  test_file <- file.path(dir, "testfile.R")
  
  # works only when package is build
  datapath <- system.file("testdata", package = "origin")
  
  test_text <- utils::read.csv2(file = file.path(datapath, "testscript.csv"),
                                na.strings = "NA",
                                encoding = "UTF-8",
                                stringsAsFactors = FALSE)
  
  # windows adds X.U.FEFF to the first variable name in read.csv2.
  # Therefore, the following bulletproof way to access the
  # correct variable / vector
  nms <- names(test_text)
  writeLines(test_text[, grepl("TEST", nms, fixed = TRUE)],
             con = test_file)
  
  # run function
  testthat::expect_warning(
    res <- check_pkg_usage(path = dir,
                           pkgs = c("data.table",
                                    "dplyr",
                                    "purrr"
                           ),
                           recursive = FALSE, 
                           use_markers = FALSE), 
    regexp = 
      paste0("Cannot check for local functions due to unclear rootdirectory",
             "|RStudio not running"))
  
  testthat::expect_equal(res$fun, 
                         c("as.IDate", "as.data.table", "bind_cols",
                           "c", "copy", "filter", 
                           "function", "if_else", "is.data.table",
                           "last", "last", "n", 
                           "n_distinct", "setkey", NA))
  testthat::expect_equal(res$n_calls, 
                         c(1, 1, 10, 2, 1, 7, 1, 1, 1, 3, 3, 6, 5, 3, 0))
  
})


testthat::test_that("check_pkg_usage fails - empty file", {
  dir <- tempfile()
  dir.create(dir)
  test_file_empty <- file.path(dir, "testfile_empty.R")
  
  # write an empty file
  writeLines("", con = test_file_empty)
  
  testthat::expect_warning(
    testthat::expect_error(check_pkg_usage(path = dir,
                                           pkgs = c("data.table",
                                                    "dplyr",
                                                    "purrr"
                                           ),
                                           recursive = FALSE, 
                                           use_markers = FALSE),
                           "All scripts in this directory are empty"))
})