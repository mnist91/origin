#' Find Function Names
#'
#' @param file a character string linking to a R file path
#'
#' @return a character vector
#' @noRd
find_functions <- function(file = ".", text = NULL) {

  # get information about parsed data
  dat <- get_parsed_data(file = file,
                         text = text)

  # the corresponding symbols are the function names
  fun_names <- dat[dat$usage == "FUNCTION_DEFINITION", "text"]
  fun_names <- fun_names[!is.na(fun_names)]
  
  return(fun_names)
}
