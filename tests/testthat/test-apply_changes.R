testthat::test_that("apply_changes", {

  # everything remains as before
  result <- list(to_write = letters)
  testthat::expect_message(
    testthat::expect_null(apply_changes(result = result,
                                         init_script = letters))
  )
})
