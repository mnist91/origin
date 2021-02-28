# Push dummy R script to a temp directory --------------------------------------
dir <- tempdir()
test_file_path <- file.path(dir, "testfile.R")
test_text <- readr::read_delim("../testscript.csv",
                               delim = ";",
                               trim_ws = FALSE,
                               col_names = TRUE,
                               col_types = "cc",
                               na = "NA")
writeLines(test_text$TESTSKRIPT, con = test_file_path)

# Unit tests
testthat::test_that("origin file", {
  writeLines(test_text$TESTSKRIPT, con = test_file_path)
  target <- readLines(target_file_path)

  # In einem Schritt, mit crosschecks
  addPackageToFunction_all(target_file_path,
                           pkgs = c("data.table",
                                    "dplyr",
                                    "testthat",
                                    "purrr"),
                           overwrite = TRUE,
                           ignoreComments = TRUE,
                           excludeBasePackages = TRUE,
                           verbose = FALSE)

  testfile_after <- readLines(target_file_path)

  testthat::expect_equal(testfile_after, test_text$TARGET)
})


testthat::test_that("origin iterative", {

  # Iterativ, Pakete werden unabhängig voneinander überprüft
  purrr::walk(c("data.table",
                "dplyr",
                "testthat",
                "purrr"
  ),
  .f = ~ addPackageToFunction(pkg = .x,
                              file = target_file_path,
                              overwrite = TRUE,
                              ignoreComments = TRUE,
                              verbose = TRUE)
  )
  testfile_after <- readLines(target_file_path)

  testthat::expect_equal(testfile_after, test_text$TARGET)

})
