testthat::test_that("finding leading highlight type", {

  highlight_types <- c("MISSING", "INSERT", "SPECIAL")
  testthat::expect_equal(
    set_marker_type(highlight_types, use_markers = TRUE), "info")
  testthat::expect_equal(
    set_marker_type(highlight_types, use_markers = FALSE), "+")

  highlight_types <- c("INSERT", "INSERT", "INSERT")
  testthat::expect_equal(
    set_marker_type(highlight_types, use_markers = TRUE), "info")
  testthat::expect_equal(
    set_marker_type(highlight_types, use_markers = FALSE), "+")

  highlight_types <- c("INSERT", "INSERT", "SPECIAL")
  testthat::expect_equal(
    set_marker_type(highlight_types, use_markers = TRUE), "info")
  testthat::expect_equal(
    set_marker_type(highlight_types, use_markers = FALSE), "+")

  highlight_types <- c("SPECIAL", "SPECIAL", "MISSING")
  testthat::expect_equal(
    set_marker_type(highlight_types, use_markers = TRUE), "warning")
  testthat::expect_equal(
    set_marker_type(highlight_types, use_markers = FALSE), "-")

})
