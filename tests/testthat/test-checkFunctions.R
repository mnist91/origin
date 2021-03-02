test_that("checkFunctions has correct output", {

  # example input
  script <- c(
    "nrow(x) %like%  n_row(nrow)",
    "# nrow(x)",
    "y <- x$nrow",
    "y <- no_n_row_like"
  )
  functions <- c("nrow", "%like%", "n_row")

  # expected results
  res1 <- res2 <- list(
    matches = c(TRUE, TRUE),
    lineMatches = c(TRUE, FALSE, TRUE, TRUE),
    special_matches = TRUE,
    special_functions = c(FALSE, TRUE, FALSE),
    functionsInScript = c("nrow", "n_row")
  )

  res3 <- list(
    matches = c(TRUE, TRUE),
    lineMatches = c(TRUE, TRUE, TRUE, TRUE),
    special_matches = TRUE,
    special_functions = c(FALSE, TRUE, FALSE),
    functionsInScript = c("nrow", "n_row")
  )

  # call function with different settings
  tmp1 <- checkFunctions(script,
                         functions,
                         ignoreComments = TRUE,
                         pkg = NULL,
                         verbose = TRUE)


  tmp2 <- checkFunctions(script,
                         functions,
                         ignoreComments = TRUE,
                         pkg = "this_pkg",
                         verbose = TRUE)

  tmp3 <- checkFunctions(script,
                         functions,
                         ignoreComments = FALSE,
                         pkg = NULL,
                         verbose = FALSE)

  # compare results
  expect_equal(tmp1, res1)
  expect_equal(tmp2, res2)
  expect_equal(tmp3, res3)

})
