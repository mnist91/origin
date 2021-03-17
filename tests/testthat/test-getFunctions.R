testthat::test_that("export type is right", {

  export_functions <- get_exported_functions("utils")
  testthat::expect_type(export_functions, "character")
})
