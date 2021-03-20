testthat::test_that("finding leading highlight type", {

  highlight_types <- c("missed", "insert", "special")
  testthat::expect_equal(
    set_marker_type(highlight_types, use_markers = TRUE), "warning")
  testthat::expect_equal(
    set_marker_type(highlight_types, use_markers = FALSE), "x")

  highlight_types <- c("insert", "insert", "insert")
  testthat::expect_equal(
    set_marker_type(highlight_types, use_markers = TRUE), "info")
  testthat::expect_equal(
    set_marker_type(highlight_types, use_markers = FALSE), "-")

  highlight_types <- c("insert", "insert", "special")
  testthat::expect_equal(
    set_marker_type(highlight_types, use_markers = TRUE), "box")
  testthat::expect_equal(
    set_marker_type(highlight_types, use_markers = FALSE), "o")

  highlight_types <- c("special", "special", "missed")
  testthat::expect_equal(
    set_marker_type(highlight_types, use_markers = TRUE), "warning")
  testthat::expect_equal(
    set_marker_type(highlight_types, use_markers = FALSE), "x")

})
