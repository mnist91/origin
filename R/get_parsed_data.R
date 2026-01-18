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
  is_rmd <- is_rmd_file(file)
  if (is.null(text) && is_rmd) {
    script <- suppressWarnings(readLines(file))
    r_lines <- which(extract_r_chunk_lines(script))
    script <- script[r_lines]
  } else {
    script <- text
  }
  
  if (is.null(script)) {
    script <- readLines(file)
  }
  # script <- gsub("\\", "\\\\", script, fixed = TRUE)
  
  # detailed information about parsed code
  dat <- utils::getParseData(parse(file = file,
                                   text = script,
                                   keep.source = TRUE),
                             includeText = TRUE)
  dat <- dat[dat$terminal == TRUE, # Exclude Linting - more verbose
             c("line1", "col1", "line2", "col2", "token", "text")]

  # replace wiht original line indices
  if (is.null(text) && is_rmd) {
    dat$line1 <- r_lines[dat$line1]
    dat$line2 <- r_lines[dat$line2]
  }
  # get positions where funcitons are defined
  dat <- get_function_definitions(dat)

  # get positions where funcitons are called/used
  dat <- get_function_calls(dat)

  if (!is.null(file) && nzchar(file) && nrow(dat) > 0) {
    dat$file <- file
  }

  return(dat)
}
