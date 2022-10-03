test_that("get_packages", {
  expect_true(!"origin" %in% get_packages())
})
