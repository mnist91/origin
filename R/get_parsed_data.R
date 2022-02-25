#' Retrieve Parse Information from a string
#'
#' @param x an R script as a character vector
#'
#' @return data.frame
#' @export
#'
#' @examples
#' get_parsed_data("myfun <- function(x) mean(x))
get_parsed_data <- function(file = "",
                            text = NULL) {
  # detailed information about parsed code
  dat <- getParseData(parse(file = file,
                            text = text,
                            keep.source = TRUE),
                      includeText = TRUE)
  dat <- dat[dat$terminal == TRUE,
             c("line1", "col1", "line2", "col2", "token", "text")]

  # get positions where funcitons are defined
  dat <- get_function_definitions(dat)

  # get positions where funcitons are called/used
  dat <- get_function_calls(dat)

  if (nzchar(file)) {
    dat$file <- file
  }
  return(dat)
}
