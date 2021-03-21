#' Unlist a list into a vector with names equal to fromer list element name
#'
#' The regular `base::unlist()` function creates a named vector in which each
#' element has a name corresponding to the list element it belonged to priorly
#' plus a number to yield unique names. This function basically excludes the
#' number part
#'
#' @param l a list to convert into a vector
#' @param nms character vector of same length as list, defaults to the
#'   names of l
#'
#' @details the `base::unlist` function converts a list into a vector yet
#'    assigns unique names to each vector element, More precisely, it adds
#'     a number to the name of its list element. This function does not
#'     create unique names but assigns the bare name of the list element
#'     to all vector elements that stem from this list element
#'
#' @return named vector
#' @export
#' @importFrom stats setNames
#'
#' @examples
#' l <- list(red = 1:3, blue = 1:2)
#' un_list(l)
#' # >  red  red  red blue blue
#' # >    1    2    3    1    2
un_list <- function(l, nms = names(l)) {
  out <- unlist(
    recursive = TRUE,
    use.names = TRUE,
    x = mapply(
      FUN = function(x, nm) {
        setNames(x, rep(nm, length(x)))
      },
      l,
      nms,
      SIMPLIFY = FALSE,
      USE.NAMES = FALSE
    )
  )
  return(out)
}
