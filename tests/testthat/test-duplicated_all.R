testthat::test_that("duplicated all", {
  # numeric 
  x <- c(1, 2, 1)
  testthat::expect_equal(duplicated_all(x), c(TRUE, FALSE, TRUE))

  # character 
  x <- c("a", "b", "a")
  testthat::expect_equal(duplicated_all(x), c(TRUE, FALSE, TRUE))

  # logical 
  x <- c(TRUE, FALSE, TRUE)
  testthat::expect_equal(duplicated_all(x), c(TRUE, FALSE, TRUE))

})
