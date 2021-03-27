testthat::test_that("Preparation of new file by line", {

  # created by testscript file
  # Exclude Linting
  # lapply(FUN = function(x) x[logging_comb$line %in% c(80, 30, 33)], logging_comb) %>% dput
  input_list <- list(line = c(30L, 33L, 80L, 80L, 30L, 33L, 30L, 33L, 80L),
                     string = c("iris %>% filter(Species == \"setosa\") %>% filter(Spepal.Length > 3) %>% filter(TRUE)",
                                "iris %>% filter(filter) %>% filter(Spepal.Length > 3)",
                                "map(iris, .f =bind_cols)",
                                "map(iris, .f =bind_cols)",
                                "iris %>% filter(Species == \"setosa\") %>% filter(Spepal.Length > 3) %>% filter(TRUE)",
                                "iris %>% filter(filter) %>% filter(Spepal.Length > 3)",
                                "iris %>% filter(Species == \"setosa\") %>% filter(Spepal.Length > 3) %>% filter(TRUE)",
                                "iris %>% filter(filter) %>% filter(Spepal.Length > 3)",
                                "map(iris, .f =bind_cols)"
                     ),
                     matches = list(c(10, 42, 72), c(10, 29), 15, 1,
                                    c(6, 38, 68), c(6, 25), c(10, 42, 72),
                                    c(10, 17, 29), c(1, 15)),
                     log_length = list(c(0, 0, 0), c(0, 0), 0, 0, c(3L, 3L, 3L),
                                       c(3L, 3L),
                                       c(6L, 6L, 6L), c(6L, 6L, 6L), c(3L, 9L)),
                     pkg = c("dplyr::", "dplyr::", "dplyr::", "purrr::", "", "",
                             "", "", ""),
                     type = c("insert", "insert", "insert", "insert", "special",
                              "special", "missed", "missed", "missed"))

  # use_markers == FALSE -------------------------------------------------------
  result <- list(data.frame(line = 30L,
                            message = "\033[39miris \033[33m%>%\033[39m \033[36mdplyr::\033[39mfilter(Species == \"setosa\") \033[33m%>%\033[39m \033[36mdplyr::\033[39mfilter(Spepal.Length > 3) \033[33m%>%\033[39m \033[36mdplyr::\033[39mfilter(TRUE)\033[39m",
                            type = "o",
                            column = 6),
                 data.frame(line = 33L,
                            message = "\033[39miris \033[33m%>%\033[39m \033[36mdplyr::\033[39mfilter(\033[31mfilter\033[39m) \033[33m%>%\033[39m \033[36mdplyr::\033[39mfilter(Spepal.Length > 3)\033[39m",
                            type = "x",
                            column = 6),
                 data.frame(line = 80L,
                            message = "\033[39m\033[36mpurrr::\033[39mmap(iris, .f =\033[36mdplyr::\033[39mbind_cols)\033[39m",
                            type = "-",
                            column = 1))


  testthat::expect_equal(
    object = lapply(
      X = c(30L, 33L, 80L),
      FUN = prep_line_logging,
      lines = input_list$line,
      matches = input_list$matches,
      pkg = input_list$pkg,
      log_length = input_list$log_length,
      type = input_list$type,
      string = input_list$string,
      use_markers = FALSE),
    expected = result)


  # use_markers == TRUE —-------------------------------------------------------
  opts <- options()
  options(origin.color_added_package = "#00F9FF")
  options(origin.color_missed_function  = "#ff0000")
  options(origin.color_special_function = "#ffa500")
  result <- list(data.frame(line = 30L,
                            message = "<div>iris <text style=\"color: #ffa500;\">%>%</text> <text style=\"color: #00F9FF;\">dplyr::</text>filter(Species == \"setosa\") <text style=\"color: #ffa500;\">%>%</text> <text style=\"color: #00F9FF;\">dplyr::</text>filter(Spepal.Length > 3) <text style=\"color: #ffa500;\">%>%</text> <text style=\"color: #00F9FF;\">dplyr::</text>filter(TRUE)</div",
                            type = "box",
                            column = 6),
                 data.frame(line = 33L,
                            message = "<div>iris <text style=\"color: #ffa500;\">%>%</text> <text style=\"color: #00F9FF;\">dplyr::</text>filter(<text style=\"color: #ff0000;\">filter</text>) <text style=\"color: #ffa500;\">%>%</text> <text style=\"color: #00F9FF;\">dplyr::</text>filter(Spepal.Length > 3)</div",
                            type = "warning",
                            column = 6),
                 data.frame(line = 80L,
                            message = "<div><text style=\"color: #00F9FF;\">purrr::</text>map(iris, .f =<text style=\"color: #00F9FF;\">dplyr::</text>bind_cols)</div",
                            type = "info",
                            column = 1))
  # mutliple insertions yet different packages
  testthat::expect_equal(
    object = lapply(
      X = c(30L, 33L, 80L),
      FUN = prep_line_logging,
      lines = input_list$line,
      matches = input_list$matches,
      pkg = input_list$pkg,
      log_length = input_list$log_length,
      type = input_list$type,
      string = input_list$string,
      use_markers = TRUE),
    expected = result)

  # reset options
  options(opts)

})