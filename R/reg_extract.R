#' Extract string by regular expression
#'
#' @param x a character vector
#' @param pattern a regular expression
#' @param invert  logical: if TRUE, extract or replace the
#' non-matched substrings.
#' @param ... arguments passed to \link[base]{gregexpr}
#'
#' @return a character vector
reg_extract <- function(x, pattern, invert = FALSE, ...) {
  out <- regmatches(x = x,
                    m = gregexpr(pattern = pattern,
                                 text = x,
                                 ...),
                    invert = invert)
  return(out)
}


