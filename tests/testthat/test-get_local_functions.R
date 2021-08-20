testthat::test_that("get local functions", {

  dir <- tempdir(check = TRUE)
  writeLines(c("myfun <- function(x) 3",
               "myfun2 =function () 1234",
               "myfun3 <- ",
               "function",
               "(x) 3"), con = file.path(dir, "findfuns.R"))
  result <- get_local_functions(dir)

  testthat::expect_true(all(c("myfun", "myfun2", "myfun3") %in% result))
})

