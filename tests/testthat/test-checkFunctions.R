testthat::test_that("check_functions has correct output", {

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
    matches = c(nrow = TRUE, n_row = TRUE),
    line_matches = c(TRUE, FALSE, TRUE, TRUE),
    special_matches = c(`%like%` = TRUE),
    special_functions = c(FALSE, TRUE, FALSE),
    functions_in_script = c("nrow", "n_row")
  )

  res3 <- list(
    matches = c(nrow = TRUE, n_row = TRUE),
    line_matches = c(TRUE, TRUE, TRUE, TRUE),
    special_matches = c(`%like%` = TRUE),
    special_functions = c(FALSE, TRUE, FALSE),
    functions_in_script = c("nrow", "n_row")
  )

  # call function with different settings
  tmp1 <- check_functions(script,
                         functions,
                         ignore_comments = TRUE,
                         pkg = NULL,
                         verbose = TRUE)


  tmp2 <- check_functions(script,
                         functions,
                         ignore_comments = TRUE,
                         pkg = "this_pkg",
                         verbose = TRUE)

  tmp3 <- check_functions(script,
                         functions,
                         ignore_comments = FALSE,
                         pkg = NULL,
                         verbose = FALSE)

  # compare results
  testthat::expect_equal(tmp1, res1)
  testthat::expect_equal(tmp2, res2)
  testthat::expect_equal(tmp3, res3)

})
