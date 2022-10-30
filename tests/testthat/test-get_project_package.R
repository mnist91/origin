testthat::test_that("get_project_pkg", {
  testthat::expect_equal(get_project_pkg(),
               if (!rstudioapi::isAvailable())  NULL else  "origin")
})
