testthat::test_that("package initialization", {
  # reset origin options
  nms <- names(options())
  nms <- nms[grepl("^origin\\.", nms)]
  do.call(options, setNames(lapply(nms, function(x) NULL), nms))
  testthat::expect_null(.onLoad())
  testthat::expect_null(.onLoad())

})
