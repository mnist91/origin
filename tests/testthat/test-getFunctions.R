test_that("export type is right", {

  export_functions <- getFunctions("utils")
  expect_type(export_functions, "character")
})
