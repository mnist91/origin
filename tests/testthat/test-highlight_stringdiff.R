test_that("color coding - one line", {

  prior <- "irisdt <- as.data.table(iris)"
  after <- "irisdt <- data.table::as.data.table(iris)"

  res_html <- paste0('irisdt <- <text style=\"color:red;\">',
                     'data.table::</text>as.data.table(iris)')
  res_nohtml <- "irisdt <- \033[36mdata.table::\033[39mas.data.table(iris)"

  tmp_html <- highlight_stringdiff(prior = prior, after = after, html = TRUE)
  tmp_nohtml <- highlight_stringdiff(prior = prior, after = after, html = FALSE)

  expect_equal(tmp_html, res_html)
  expect_equal(tmp_nohtml, res_nohtml)

})
