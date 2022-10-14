testthat::test_that("Invoke Logging", {

  # Console logging
  dat <- data.frame(file = "A",
                    line = 1,
                    type = "x",
                    message = "mutate(dat)",
                    stringsAsFactors = FALSE)

  # suppress print output
  utils::capture.output(
    testthat::expect_null(
      run_logging(dat, use_markers = FALSE)
    )
  )

  # Markers logging
  dat <- data.frame(file = ".",
                    line = 1,
                    type = "info",
                    message = "mutate(dat)",
                    stringsAsFactors = FALSE)

  # throws error due to missing column variable, hence no Markers output
  testthat::expect_error(
    testthat::expect_null(
      run_logging(dat, use_markers = TRUE)
    )
  )


  # no logging
  testthat::expect_message(
    run_logging(data.frame())
  )
})
