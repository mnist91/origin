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

  testthat::expect_error(
    check_pkg_usage(path = dir,
                    pkgs = "unknown_package",
                    recursive = FALSE,
                    use_markers = FALSE),
    regexp = "uninstalled packages")

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
                           "filter", "if_else", "mutate",
                           "n", "n_distinct", "map",
                           "bind_cols_2", NA))
  testthat::expect_equal(res$n_calls,
                         c(2, 10, 1, 2, 2, 1, 1, 1, 1, 1, 4, 3,
                           3, 7, 10, 8, 1, 3, 6,
                           5, 4, 1, 0))

  # run function with markers
  # give specific directory for local functisn to avoid warning
  dir2 <- tempfile()
  dir.create(dir2)
  writeLines("myfun <- function(x) 123",
             con = file.path(dir2, "functions_file.R"))
  testthat::expect_warning(
    res <- check_pkg_usage(path = dir,
                           pkgs = c("data.table",
                                    "data.table",
                                    "testthat"),
                           recursive = FALSE,
                           use_markers = TRUE,
                           path_to_local_functions = dir2),
    "The following packages are provided more than once:")

  testthat::expect_snapshot(print(res))

})


testthat::test_that("check_pkg_usage working on target", {
  dir <- tempfile()
  dir.create(dir)
  test_file <- file.path(dir, "testfile.R")
  test_file_empty <- file.path(dir, "testfile_empty.R")

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


  # write an empty file
  writeLines(c("", "", ""), con = test_file_empty)

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
                           "if_else", "mutate", "n",
                           "n_distinct", "map", "%>%", "bind_cols_2"
                         ))

  testthat::expect_snapshot(res)
})


testthat::test_that("check_pkg_usage fails - no R files", {
  dir <- tempfile()
  dir.create(dir)

  testthat::expect_error(check_pkg_usage(path = dir,
                                         pkgs = c("data.table",
                                                  "dplyr",
                                                  "purrr"
                                         ),
                                         recursive = FALSE,
                                         use_markers = FALSE),
                         "No R files in")
})

testthat::test_that("check_pkg_usage fails - excluded files", {
  dir <- tempfile()
  dir.create(dir)
  test_file_empty <- file.path(dir, "testfile_empty.R")

  # write an empty file
  writeLines(character(3), con = test_file_empty)

  testthat::expect_error(check_pkg_usage(path = dir,
                                         pkgs = c("data.table",
                                                  "dplyr",
                                                  "purrr"
                                         ),
                                         exclude_files = test_file_empty,
                                         recursive = FALSE,
                                         use_markers = FALSE),
                         "All R files excluded")
  testthat::expect_error(check_pkg_usage(path = dir,
                                         pkgs = c("data.table",
                                                  "dplyr",
                                                  "purrr"
                                         ),
                                         exclude_files = "test_file_empty.R",
                                         recursive = FALSE,
                                         use_markers = FALSE),
                         "File to exclude not in given path")
})

testthat::test_that("check_pkg_usage fails - empty files", {
  dir <- tempfile()
  dir.create(dir)
  test_file_empty <- file.path(dir, "testfile_empty.R")

  # write an empty file
  writeLines(character(3), con = test_file_empty)

  testthat::expect_message(
    testthat::expect_equal(
      check_pkg_usage(path = dir,
                      pkgs = c("data.table",
                               "dplyr",
                               "purrr"
                      ),
                      recursive = FALSE,
                      use_markers = FALSE),
      NULL),
    "All provided files are empty")
})

testthat::test_that("check_pkg_usage fails - Path_tolocal_functions", {
  dir <- tempfile()
  dir.create(dir)
  test_file_empty <- file.path(dir, "testfile_empty.R")

  # write a file to not tigger ''No R Files' Error
  writeLines("blubb", con = test_file_empty)

  testthat::expect_error(
    check_pkg_usage(path = dir,
                    pkgs = c("data.table",
                             "dplyr",
                             "purrr"
                    ),
                    path_to_local_functions = "path_to_local_functions.R"),
    "Given path_to_local_functions.+does not exist.")
})
