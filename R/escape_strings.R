#' Escape infix regex characters
#'
#' @param x character vector with strings to be used in a regex
#'
#' @return a character vector
#' @noRd
escape_strings <- function(x) {
  strings_to_escape <- c("*", ".", "?", "^", "+", "$", "|", "(", ")", "[",
                         "]", "{", "}", "\\", "-")
  out <- gsub(
    paste0("([\\", paste(collapse = "\\", strings_to_escape), "])"),
    "\\\\\\1",
    x,
    perl = TRUE)

  return(out)

}
