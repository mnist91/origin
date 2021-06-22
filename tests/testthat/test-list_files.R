testthat::test_that("excluding folders in list_files works", {
  dir <- tempfile()
  dir.create(dir)

  subdir1 <- file.path(dir, "sub1")
  subdir2 <- file.path(dir, "sub2")

  dir.create(subdir1)
  dir.create(subdir2)

  writeLines(text = LETTERS, con = file.path(dir, "tmp_root.R"))
  writeLines(text = LETTERS, con = file.path(subdir1, "tmp_sub1.R"))
  writeLines(text = LETTERS, con = file.path(subdir2, "tmp_sub2.R"))

  # one folder
  expect_equal(sort(list_files(path = dir,
                               exclude_folders = "sub2",
                               full.names = TRUE,
                               include.dirs = FALSE,
                               recursive = TRUE,
                               pattern = "\\.R$",
                               ignore.case = TRUE)),
               sort(c(file.path(dir, "tmp_root.R"),
                      file.path(subdir1, "tmp_sub1.R"))))

  # multiple folders
  expect_equal(sort(list_files(path = dir,
                               exclude_folders = c("sub1", "sub2"),
                               full.names = TRUE,
                               include.dirs = FALSE,
                               recursive = TRUE,
                               pattern = "\\.R$",
                               ignore.case = TRUE)),
               file.path(dir, "tmp_root.R"))

  # no folders
  expect_equal(sort(list_files(path = dir,
                               exclude_folders = NULL,
                               full.names = TRUE,
                               include.dirs = FALSE,
                               recursive = TRUE,
                               pattern = "\\.R$",
                               ignore.case = TRUE)),
               sort(c(file.path(dir, "tmp_root.R"),
                      file.path(subdir1, "tmp_sub1.R"),
                      file.path(subdir2, "tmp_sub2.R"))))
})
