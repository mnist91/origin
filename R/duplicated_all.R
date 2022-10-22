#' Determine all duplicated values in a vector
#' 
#' @description  \link[base]{duplicated}() only returns `TRUE` for the 
#'  second value, i.e. the first duplicate. 
#'
#' @param x a vector
#'
#' @return a logical vector of the same length as x.
#' `TRUE` marks all duplicated values
#' @noRd
#'
#' @examples
#' duplicated_all(c(1, 2, 1))
duplicated_all <- function(x) {
  x %in% x[duplicated(x)]
}
