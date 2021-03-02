test_that("color coding - multiple lines", {

  prior_list <- list(
    "irisdt <- as.data.table(iris)",
    "carsdt <- data.table::as.data.table(cars)",
    "carsdt <- merge.data.table(cars)",
    "a %like% b %>% nrow(nrows)")
  after_list <- list(
    "irisdt <- data.table::as.data.table(iris)",
    "carsdt <- data.table::as.data.table(cars)",
    "carsdt <- data.table::merge.data.table(cars)",
    "a %like% b %>% dplyr::nrow(nrows)")

  res_html <- list(
    "irisdt <- <text style=\"color:red;\">data.table::</text>as.data.table(iris)",
    "carsdt <- data.table::as.data.table(cars)",
    "carsdt <- <text style=\"color:red;\">data.table::</text>merge.data.table(cars)",
    "a %like% b %>% <text style=\"color:red;\">dplyr::</text>nrow(nrows)"
  )
  res_nohtml <- list(
    "irisdt <- \033[36mdata.table::\033[39mas.data.table(iris)",
    "carsdt <- data.table::as.data.table(cars)",
    "carsdt <- \033[36mdata.table::\033[39mmerge.data.table(cars)",
    "a %like% b %>% \033[36mdplyr::\033[39mnrow(nrows)"
  )


  tmp_html <- highlight_stringdiff_iter(prior = prior_list,
                                        after = after_list,
                                        html = TRUE)
  tmp_nohtml <- highlight_stringdiff_iter(prior = prior_list,
                                          after = after_list,
                                          html = FALSE)

  expect_equal(tmp_html, res_html)
  expect_equal(tmp_nohtml, res_nohtml)

})
