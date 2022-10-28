testthat::test_that("check_pkg_usage() working on test", {
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
                                    "purrr",
                                    "testthat"),
                           recursive = FALSE, 
                           use_markers = FALSE), 
    regexp = 
      paste0("Cannot check for local functions due to unclear root directory",
             "|RStudio not running"))

  testthat::expect_equal(res$fun, 
                         c("c", "lapply", "paste", "%between%", "%like%",
                           ":=", "as.IDate", "as.data.table", 
                           "copy", "is.data.table", "last", 
                           "setkey", "setkey", "%>%", "bind_cols", 
                           "filter", "id", "if_else", "mutate",
                           "n", "n_distinct", "map", 
                           "bind_cols_2", NA))
  testthat::expect_equal(res$n_calls, 
                         c(2, 10, 1, 2, 2, 1, 1, 1, 1, 1, 4, 3,
                           3, 7, 10, 7, 1, 1, 3, 6, 
                           5, 4, 1, 0))
  
  # run function with markers
  testthat::expect_warning(
    res <- check_pkg_usage(path = dir,
                           pkgs = c("data.table",
                                    "testthat"),
                           recursive = FALSE, 
                           use_markers = TRUE), 
    regexp = 
      paste0("Cannot check for local functions due to unclear root directory",
             "|RStudio not running"))
  
  testthat::expect_snapshot(print(res))
  
})


testthat::test_that("check_pkg_usage working on target", {
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
  writeLines(test_text[, grepl("TARGET", nms, fixed = TRUE)],
             con = test_file)
  
  # run function with markers
  testthat::expect_warning(
    res <- check_pkg_usage(path = dir,
                           pkgs = c("data.table",
                                    "testthat"),
                           recursive = FALSE, 
                           use_markers = TRUE), 
    regexp = 
      paste0("Cannot check for local functions due to unclear root directory",
             "|RStudio not running"))

  testthat::expect_equal(res$fun, 
                         c("c", "lapply", "paste", "%between%", "%like%",
                           ":=", "as.IDate", "as.data.table", 
                           "copy", "is.data.table", "last",
                           "setkey", "bind_cols", "filter", 
                           "id", "if_else", "mutate", "n",
                           "n_distinct", "map", "%>%", "bind_cols_2"
                         ))

  testthat::expect_snapshot(res)
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

