# Push dummy R script to a temp directory --------------------------------------
dir <- tempdir()
test_file_path <- file.path(dir, "testfile.R")
target_file_path <- file.path(dir, "targetfile.R")

# works only when package is build
# datapath <- system.file("testdata", package = "origin")

datapath <- file.path("inst", "testdata")

#datapath <- file.path("inst", "testdata")
test_text <- read.csv2(file = file.path(datapath, "testscript.csv"),
                       na.strings = "NA")
writeLines(test_text$TARGET, con = target_file_path)


# nur für renv um zum testen verfügbar zu sein
library("data.table", include.only = NULL)
library("dplyr", include.only = NULL)

# only until the package is build, then this line can be removed
invisible(lapply(list.files("R", full.names = TRUE), FUN = source))

# Unit tests
testthat::test_that("origin file", {
  writeLines(test_text$TESTSKRIPT, con = test_file_path)
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
                   ignoreComments = TRUE,
                   verbose = TRUE)
  # )
  
  testfile_after <- readLines(test_file_path)
  
  testthat::expect_equal(testfile_after, test_text$TARGET)
})


datapath <- file.path("inst", "testpath")
originize_dir(datapath,
              pkgs = c("data.table",
                       "dplyr",
                       "testthat",
                       "purrr"),
              overwrite = TRUE,
              ignoreComments = TRUE,
              verbose = FALSE)
