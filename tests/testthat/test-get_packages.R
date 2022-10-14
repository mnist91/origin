test_that("get_packages", {
  expect_true(if (!rstudioapi::isAvailable()) {
    "origin" %in% get_packages()
  } else {
    !"origin" %in% get_packages()
  })
  
})
