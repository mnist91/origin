testthat::test_that("finding leading highlight type", {

  highlight_types <- c("missed", "insert", "infix")
  testthat::expect_equal(
    set_marker_type(highlight_types, use_markers = TRUE), "warning")
  testthat::expect_equal(
    set_marker_type(highlight_types, use_markers = FALSE), "-")

  highlight_types <- c("insert", "insert", "insert")
  testthat::expect_equal(
    set_marker_type(highlight_types, use_markers = TRUE), "info")
  testthat::expect_equal(
    set_marker_type(highlight_types, use_markers = FALSE), "+")

  highlight_types <- c("insert", "insert", "infix")
  testthat::expect_equal(
    set_marker_type(highlight_types, use_markers = TRUE), "box")
  testthat::expect_equal(
    set_marker_type(highlight_types, use_markers = FALSE), "i")

  highlight_types <- c("infix", "infix", "missed")
  testthat::expect_equal(
    set_marker_type(highlight_types, use_markers = TRUE), "warning")
  testthat::expect_equal(
    set_marker_type(highlight_types, use_markers = FALSE), "-")

})
