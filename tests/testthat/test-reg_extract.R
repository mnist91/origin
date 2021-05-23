testthat::test_that("reg_extract", {

  # additional arguments to gregexpr
  s1 <- "This is a string"
  result <- reg_extract(x = s1,
                        pattern = "this",
                        perl = TRUE,
                        ignore.case = TRUE)
  testthat::expect_equal(result, expected = list("This"))

  # invert
  s2 <- "This thing is a thing string"
  result <- reg_extract(x = s2,
                        pattern = "thing",
                        invert = TRUE,
                        perl = TRUE)
  testthat::expect_equal(result, expected = list(c("This ", " is a ", " string")))


  # invert match at the beginning causes empty string
  s3 <- "This is a string"
  result <- reg_extract(x = s3,
                        pattern = "This",
                        invert = TRUE,
                        perl = TRUE)
  testthat::expect_equal(result, expected = list(c("", " is a string")))


  # Script like strings
  funs <- c("
x <- y
  myfun =
  function(x) 3
  another.fun <- function(qwertz) 4
  antoher this9.my.my_fun999.99 = function(sdfklsf)

  # splitted
  antoher this9.my.my_fun999.992 =
  function
  (sdfklsf)

  here_fun <- funny(3)

  huge_fun <-      function    (x, y, z) {
  123
  }

",
    "
x <- y
  thisfun <-
  function(x) 3
  aaskjrfun1 = function(qwertz) 4
  aaskjrfun2 =function(qwertz) 4
  aaskjrfun3 =        function(qwertz) 4
")

  result <- reg_extract(x = funs,
                        pattern = "[\\w.]+(?=[[:space:]]*(<-|<<-|=)[[:space:]]*function[[:space:]]*\\()",
                        perl = TRUE)
  testthat::expect_equal(result,
                         list(c("myfun", "another.fun", "this9.my.my_fun999.99", "this9.my.my_fun999.992", "huge_fun"),
                              c("thisfun", "aaskjrfun1", "aaskjrfun2", "aaskjrfun3")))

})


