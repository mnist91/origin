#' Find function names
#'
#' @param x a character vector
#'
#' @return a character vector
find_functions <- function(x) {

  # remove whitespace on the left
  x <- sub(pattern = "^[[:space:]]+", replacement = "", x = x, perl = TRUE)

  # detect comments
  comment <- startsWith(x = x,
                        prefix = "#")

  # exclude commented lines
  x <- x[!comment]

  # collapse string together to get function definitions over two lines
  # Begin Exclude Linting
  # myfun <-
  #   function() ...
  # End Exclude Linting
  s <- paste(x, collapse = "\n")

  # find function names
  fun_names <-
    unlist(
      use.names = FALSE,
      reg_extract(
        s,
        pattern = "[\\w.]+(?=[[:space:]]*(<-|<<-|=)[[:space:]]*function[[:space:]]*\\()",
        perl = TRUE))

  return(fun_names)
}

