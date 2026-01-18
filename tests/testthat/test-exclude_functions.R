testthat::test_that("Exclude Functions named and unnamed", {
  funlist <- list(dplyr = c("mutate", "filter", "summarize"),
                  stats = c("aggregate", "filter"))

  # exclude single specified function
  excl_list <- list(dplyr = "filter")
  testthat::expect_equal(exclude_functions(funs = funlist,
                                           to_exclude = excl_list),
                         list(dplyr = c("mutate", "summarize"),
                              stats = c("aggregate", "filter")))

  # exclude single specified function
  excl_list <- list(stats = "filter")
  testthat::expect_equal(exclude_functions(funs = funlist,
                                           to_exclude = excl_list),
                         list(dplyr = c("mutate", "filter", "summarize"),
                              stats = "aggregate"))

  # exclude multiple specified functions
  excl_list <- list(dplyr = c("filter", "mutate"))
  testthat::expect_equal(exclude_functions(funs = funlist,
                                           to_exclude = excl_list),
                         list(dplyr = "summarize",
                              stats = c("aggregate", "filter")))

  # exclude multiple functions of two packages
  excl_list <- list(stats = "filter", dplyr = "mutate")
  testthat::expect_equal(exclude_functions(funs = funlist,
                                           to_exclude = excl_list),
                         list(dplyr = c("filter", "summarize"),
                              stats = "aggregate"))

  # exclude unspecified function
  excl_list <- list("filter")
  testthat::expect_equal(exclude_functions(funs = funlist,
                                           to_exclude = excl_list),
                         list(dplyr = c("mutate", "summarize"),
                              stats = "aggregate"))

  # exclude specific and unspecified function
  excl_list <- list(dplyr = "filter", "aggregate")
  testthat::expect_equal(exclude_functions(funs = funlist,
                                           to_exclude = excl_list),
                         list(dplyr = c("mutate", "summarize"),
                              stats = "filter"))

  # exclude multiple unspecified functions
  excl_list <- list("filter", "aggregate")
  testthat::expect_equal(exclude_functions(funs = funlist,
                                           to_exclude = excl_list),
                         list(dplyr = c("mutate", "summarize"),
                              stats = character()))

  # exclude mutltiple specific functions by several calls
  excl_list <- list(dplyr = "filter", dplyr = "mutate") # Exclude Linting - by design duplicate argument
  testthat::expect_equal(exclude_functions(funs = funlist,
                                           to_exclude = excl_list),
                         list(dplyr = "summarize",
                              stats = c("aggregate", "filter")))

  # exclude mutltiple specific functions by several calls and issue warning
  excl_list <- list(dplyr = "filter", dplyr = "setDT") # Exclude Linting - by design duplicate argument
  testthat::expect_warning(
    testthat::expect_equal(exclude_functions(funs = funlist,
                                             to_exclude = excl_list),
                           list(dplyr = c("mutate", "summarize"),
                                stats = c("aggregate", "filter"))))


})
