testthat::test_that("Exclude Specific Functions", {
  funvec <- c(dplyr = "mutate",
              dplyr = "filter",
              dplyr = "summarize")

  # exclude single function
  excl_list <- list(dplyr = "filter")
  testthat::expect_equal(setdiff_specific(funs = funvec,
                                          pkg = names(funvec),
                                          excl = excl_list),
                         c(dplyr = "mutate",
                           dplyr = "summarize"))

  # try to exlcude from another package
  excl_list <- list(stats = "filter")
  testthat::expect_equal(setdiff_specific(funs = funvec,
                                          pkg = names(funvec),
                                          excl = excl_list),
                         c(dplyr = "mutate",
                           dplyr = "filter",
                           dplyr = "summarize"))

  # exclude multiple functions in one vector
  excl_list <- list(dplyr = c("filter", "mutate"))
  testthat::expect_equal(setdiff_specific(funs = funvec,
                                          pkg = names(funvec),
                                          excl = excl_list),
                         c(dplyr = "summarize"))

  # exclude multiple functions in two elements
  excl_list <- list(stats = "filter", dplyr = "mutate")
  testthat::expect_equal(setdiff_specific(funs = funvec,
                                          pkg = names(funvec),
                                          excl = excl_list),
                         c(dplyr = "filter",
                           dplyr = "summarize"))

  # exclude multiple functions in two elements
  excl_list <- list(dplyr = "filter", dplyr = "mutate")
  testthat::expect_equal(setdiff_specific(funs = funvec,
                                          pkg = names(funvec),
                                          excl = excl_list),
                         c(dplyr = "summarize"))

  # issue a warning due to a non-exported function from desired package
  excl_list <- list(dplyr = "filter", dplyr = "setDT")
  testthat::expect_warning(
    testthat::expect_equal(setdiff_specific(funs = funvec,
                                            pkg = names(funvec),
                                            excl = excl_list),
                           c(dplyr = "mutate",
                             dplyr = "summarize")))


})
