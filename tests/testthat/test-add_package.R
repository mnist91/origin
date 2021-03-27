testthat::test_that("Insert packages in a single line where needed", {

  # created by testscript file
  # code:
  # Exclude Linting
  # lapply(FUN = function(x) x[combined$line %in% c(80, 30)], combined)
  input_list <- list(string = list("map(iris, .f =bind_cols)",
                                   "map(iris, .f =bind_cols)",
                                   "map(iris, .f =bind_cols)"),
                     insertions = list(c(`purrr::` = 1, `dplyr::` = 15),
                                       c(`purrr::` = 1, `purrr::` = 15),
                                       c(`DUMMY` = 10)),
                     result = list("purrr::map(iris, .f =dplyr::bind_cols)",
                                   "purrr::map(iris, .f =purrr::bind_cols)",
                                   "map(iris,DUMMY .f =bind_cols)"
                     ))


  testthat::expect_equal(
    object = Map(add_package,
                 string = input_list$string,
                 splits = input_list$insertions,
                 pkg = lapply(input_list$insertions, names)),
    expected = input_list$result)

})
