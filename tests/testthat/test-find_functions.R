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
    "antoher this9.my.my_fun999.992 =",
    "function",
    "",
    "(sdfklsf)",
    # <- two lines
    "  thisfun <-",
    "  function(x) 3",
    "",


    # crazy function name
    "this9.my.my_fun999.99 = function(sdfklsf)",
    "{this9.my.my_fun999.99 = function(sdfklsf)}",

    # just a function call
    "here_fun <- funny(3)",
    "",

    # different kinds of spaces
    "  aaskjrfun1= function(qwertz) 4",
    "  aaskjrfun2 =function(qwertz) 4",
    "  aaskjrfun3 =        function(qwertz) 4",
    "huge_fun <-      function    (x, y, z) {",
    "123",
    "}"
  )

  result <- find_functions(x = script)
  testthat::expect_equal(result,
                         c("another.fun1",
                           "another_fun2",
                           "anotherfun3",
                           "another.fun1",
                           "another_fun2",
                           "anotherfun3",
                           "myfun",
                           "this9.my.my_fun999.992",
                           "thisfun",
                           "this9.my.my_fun999.99",
                           "this9.my.my_fun999.99",
                           "aaskjrfun1",
                           "aaskjrfun2",
                           "aaskjrfun3",
                           "huge_fun"
                         ))

})
