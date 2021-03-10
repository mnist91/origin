test_that("check verbolize", {

  script_prior <- c(
    "nrow(x) %like%  n_row(nrow)",
    "# nrow(x)",
    "y <- x$nrow",
    "y <- n_row(a)",
    "y <- no_n_row_like"
  )
  functions <- c("nrow", "%like%", "n_row", "n_col")

  script_after <- c(
    "pkg::nrow(x) %like%  pkg::n_row(nrow)",
    "# nrow(x)",
    "y <- x$nrow",
    "y <- pkg::n_row(a)",
    "y <- no_n_row_like"
  )

  # call function with different settings
  tmp <- checkFunctions(script_prior,
                        functions,
                        ignoreComments = TRUE,
                        pkg = "pkg",
                        verbose = TRUE)

  res <- c(
    "\033[32m2 Lines changed",
    "\033[39m\033[32m2 Functions recognized",
    "\033[39mnrow",
    "n_row ",
    "\033[32mChanges:",
    "\033[39mLine 1: \033[36mpkg::\033[39mnrow(x) %like%  \033[36mpkg::\033[39mn_row(nrow)",
    "Line 4: y <- \033[36mpkg::\033[39mn_row(a) ",
    "",
    " \033[34mFunction names are not used like functions. Check for variable names or functional programming in *apply/purrr\033[39m ",
    "Line 1: pkg::nrow(x) %like%  pkg::n_row(\033[33mnrow\033[39m)",
    "Line 3: y <- x$\033[33mnrow\033[39m",
    "Line 4: y <- no_\033[33mn_row\033[39m_like",
    "Line 5: pkg::nrow(x) %like%  pkg::n_row(\033[33mnrow\033[39m) ",
    "",
    " \033[35mSpecial functions used!\033[39m ",
    "Line 1: %like%\tpkg::nrow(x) %like%  pkg::n_row(nrow) ",
    "",
    ""
  )

  tmp_out <- capture.output(
    verbolize(script_prior = script_prior,
              script_after = script_after,
              line_matches = tmp$line_matches,
              functions = functions,
              functions_in_script = tmp$functions_in_script,
              special_functions = tmp$special_functions,
              special_matches = tmp$special_matches)
  )

  expect_equal(tmp_out, res)

})
