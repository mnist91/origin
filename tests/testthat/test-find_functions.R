testthat::test_that("Test find functions", {

  # Script-like character vector
  script <- c(

    # regular assignment of an object
    "x <- y",

    # <- regular dot
    "another.fun1 <- function(qwertz) 4",
    # <- regular underscore
    "another_fun2 <- function(qwertz) 4",
    # <- regular none
    "anotherfun3 <- function(qwertz) 4",

    # = regular dot
    "another.fun1 = function(qwertz) 4",
    # = regular underscore
    "another_fun2 = function(qwertz) 4",
    # = regular none
    "anotherfun3 = function(qwertz) 4",


    # = two lines
    "myfun =",
    "function(x) 3",
    # = multiple lines
    "antoher
    this9.my.my_fun999.992 =",
    "function",
    "",
    "(sdfklsf)",
    # <- two lines
    "  thisfun <-",
    "  function(x) 3",
    "",


    # crazy function name
    "this9.my.my_fun999.99 = function(sdfklsf) 3",
    "{this9.my.my_fun999.99 = function(sdfklsf) 4}",

    # just a function call
    "here_fun <- funny(3)",
    "",

    # different kinds of spaces
    "  aaskjrfun1= function(qwertz) 4",
    "  aaskjrfun2 =function(qwertz) 4",
    "  aaskjrfun3 =        function(qwertz) 4",
    "huge_fun <-      function    (x, y, z) {",
    "123",
    "}",
    # anonymous functions are introduced with R Version 4.1
    if (getRversion() >= 4.1) {
    "myfun = function(x = 3) {1
      anotherfun <- function() 3
      }
      blabb <- 3
      lapply(1:3, FUN = function(x) 3)
      thisfun = \\() 3
      blubb
      thafun <- \\() 3
      "
      } else {
    "myfun = function(x = 3) {1
      anotherfun <- function() 3
      }
      blabb <- 3
      lapply(1:3, FUN = function(x) 3)
      thisfun = function() 3
      blubb
      thafun <- function() 3
      "
      }
  )

  result <- find_functions(text = script)
  testthat::expect_equal(result[1], "another.fun1")
  testthat::expect_equal(result[2], "another_fun2")
  testthat::expect_equal(result[3], "anotherfun3")
  testthat::expect_equal(result[4], "another.fun1")
  testthat::expect_equal(result[5], "another_fun2")
  testthat::expect_equal(result[6], "anotherfun3")
  testthat::expect_equal(result[7], "myfun")
  testthat::expect_equal(result[8], "this9.my.my_fun999.992")
  testthat::expect_equal(result[9], "thisfun")
  testthat::expect_equal(result[10], "this9.my.my_fun999.99")
  testthat::expect_equal(result[11], "this9.my.my_fun999.99")
  testthat::expect_equal(result[12], "aaskjrfun1")
  testthat::expect_equal(result[13], "aaskjrfun2")
  testthat::expect_equal(result[14], "aaskjrfun3")
  testthat::expect_equal(result[15], "huge_fun")
  testthat::expect_equal(result[16], "myfun")
  testthat::expect_equal(result[17], "anotherfun")
  testthat::expect_equal(result[18], "thisfun")
  testthat::expect_equal(result[19], "thafun")


  testthat::expect_equal(find_functions(text = ""), character(0))
})
