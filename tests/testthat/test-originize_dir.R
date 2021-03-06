testthat::test_that("solve_local_duplicates triggers the expected messages", {

  dir <- tempfile()
  dir.create(dir)
  test_file_path1 <- file.path(dir, "testfile1.R")
  test_file_path2 <- file.path(dir, "testfile2.R")
  test_file_path3 <- file.path(dir, "testfile_empty.R")
  target_file_path1 <- file.path(dir, "targetfile1.R")
  target_file_path2 <- file.path(dir, "targetfile2.R")

  # works only when package is build
  datapath <- system.file("testdata", package = "origin")

  test_text <- read.csv2(file = file.path(datapath, "testscript.csv"),
                         na.strings = "NA",
                         encoding = "UTF-8",
                         stringsAsFactors = FALSE)

  # windows adds X.U.FEFF to the first variable name in read.csv2.
  # Therefore, the following bulletproof way to access the correct variable / vector
  nms <- names(test_text)
  writeLines(test_text[1:20, grepl("TARGET", nms, fixed = TRUE)], con = target_file_path1)
  writeLines(test_text[21:nrow(test_text), grepl("TARGET", nms, fixed = TRUE)], con = target_file_path2)
  writeLines(test_text[1:20, grepl("TESTSKRIPT", nms, fixed = TRUE)], con = test_file_path1)
  writeLines(test_text[21:nrow(test_text), grepl("TESTSKRIPT", nms, fixed = TRUE)], con = test_file_path2)


  # write an empty file
  writeLines(character(), con = "check.R")

  # In einem Schritt, mit crosschecks
  originize_dir(dir,
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
                check_local_funs = TRUE,
                verbose = FALSE)

  testfile_after1 <- readLines(test_file_path1)
  testfile_after2 <- readLines(test_file_path2)

  testthat::expect_equal(testfile_after1,
                         test_text[1:20, grepl("TARGET", nms, fixed = TRUE)])
  testthat::expect_equal(testfile_after2,
                         test_text[21:nrow(test_text), grepl("TARGET", nms, fixed = TRUE)])


})
