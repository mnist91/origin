#' @param excluded_functions a list. Either an unnamed list of function names
#'   as strings. These functions are excluded from all packages and never
#'   considered in origin. Or a named list with character vectors, Then
#'   the name of the list element refers to a package and the given functions
#'   are only excluded from this package. A very explicit way to handle
#'   namespace conflicts or highlighting popular infix functions like
#'   `\%>\%` or `:=`.
