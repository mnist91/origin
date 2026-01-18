testthat::test_that("package initialization", {

  # reset origin options
  nms <- names(options())
  nms <- nms[startsWith(x = nms, prefix = "origin.")]
  do.call(options, stats::setNames(lapply(nms, function(x) NULL), nms))
  testthat::expect_null(.onLoad())
  testthat::expect_null(.onLoad())

})
