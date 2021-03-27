testthat::test_that("Preparation of new file by line", {

  # created by testscript file
  # Exclude Linting: lapply(FUN = function(x) x[combined$line %in% c(80, 30)], combined) %>% dput
  input_list <- list(line = c(30L, 80L, 80L),
                     string = c("iris %>% filter(Species == \"setosa\") %>% filter(Spepal.Length > 3) %>% filter(TRUE)",
                                "map(iris, .f =bind_cols)",
                                "map(iris, .f =bind_cols)"),
                     matches = list(c(10, 42, 72), 15, 1),
                     log_length = list(c(0, 0, 0), 0, 0),
                     pkg = c("dplyr::", "dplyr::", "purrr::"),
                     type = c("insert", "insert", "insert"))

  # expected result
  result <- data.frame(
    line = c(30L, 80L),
    string = c('iris %>% dplyr::filter(Species == "setosa") %>% dplyr::filter(Spepal.Length > 3) %>% dplyr::filter(TRUE)',
               "purrr::map(iris, .f =dplyr::bind_cols)"),
    stringsAsFactors = FALSE)

  # multiple insertions yet same package
  testthat::expect_equal(
    object = prep_line_originize(line = 30L,
                                 lines = input_list$line,
                                 matches = input_list$matches,
                                 pkg = input_list$pkg,
                                 string = input_list$string),
    expected = result[1, ])

  # mutliple insertions yet different packages
  testthat::expect_equal(
    object = prep_line_originize(line = 80L,
                                 lines = input_list$line,
                                 matches = input_list$matches,
                                 pkg = input_list$pkg,
                                 string = input_list$string),
    # set rownames specifically to avoid attributes mismatching
    expected = `attr<-`(result[2, ], "row.names", 1L))

})
