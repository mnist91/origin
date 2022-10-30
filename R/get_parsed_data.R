#' Retrieve Parse Information from a string
#'
#' @param file File path to the file to parse.
#' @param text an R script as a character vector.
#'
#' @return data.frame
#' @noRd
#' @importFrom utils getParseData
#'
#' @examples
#' get_parsed_data("myfun <- function(x) mean(x)")
get_parsed_data <- function(file = "",
                            text = NULL) {
  # detailed information about parsed code
  dat <- utils::getParseData(parse(file = file,
                                   text = text,
                                   keep.source = TRUE),
                             includeText = TRUE)
  dat <- dat[dat$terminal == TRUE,
             c("line1", "col1", "line2", "col2", "token", "text")]

  # get positions where funcitons are defined
  dat <- get_function_definitions(dat)

  # get positions where funcitons are called/used
  dat <- get_function_calls(dat)

  if (!is.null(file) && nzchar(file) && nrow(dat) > 0) {
    dat$file <- file
  }

  return(dat)
}
