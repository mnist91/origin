#' Find Function Names
#'
#' @param file a character string linking to a R file path
#'
#' @return a character vector
#' @noRd
find_functions <- function(file) {

  # get information about parsed data
  dat <- get_parsed_data(file = file)

  # the corresponding symbols are the function names
  fun_names <- dat[dat$usage == "FUNCTION_DEFINITION", "text"]

  return(fun_names)
}
