testthat::test_that("export type is right", {

  export_functions <- get_exported_functions(pkg = "base")
  testthat::expect_type(export_functions, "character")
})


testthat::test_that("export is correct", {

  export_functions <- get_exported_functions(pkg = "base")
  testthat::expect_equal(export_functions[1:5], c("!", "$", "&", "(", "*"))
  
  
  export_functions <- get_exported_functions(pkg = "purrr")
  testthat::expect_true(all(c("map", "walk", "imap", "pmap") %in%
                              export_functions))
  
  
})
