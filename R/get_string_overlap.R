#' Returns the Starting Overlap of Multiple Strings
#'
#' @param x a character vector
#'
#' @return a character string that is the overlap of all elements of x. An empty
#'   string `""` if no overlap exists.
#' @noRd
#'
#' @examples
#' x <- c("aabbccdd", "aabbc", "aabxyzddxyz")
#' get_string_overlap(x)
#' #> [1] "aab
get_string_overlap <- function(x) {

  # return empty string if any given string is empty
  # hence no overlap is possible
  if (any(!nzchar(x))) {
    return("")
  }

  # return NA if all elements of the vector are NA
  if (all(x_na <- is.na(x))) {
    return(NA_character_)
  }

  # keep non-NA values only
  x <- unique(x[!x_na])


  # find string length in vector
  min_char <- min(unlist(lapply(X = x, FUN = nchar)))

  # make all strings the same length
  x <- unique(substr(x = x,
                     start = 1,
                     stop = min_char))

  # tokenize each string, i.e. split them into single characters
  x_tokenized <- strsplit(x, "")

  # Combine tokens sequentially by position
  combs <- matrix(data = unlist(x_tokenized),
                  nrow = min_char,
                  ncol = length(x_tokenized),
                  byrow = FALSE)

  # get positions where all strings are identical
  overlap <- apply(X = combs,
                   MARGIN = 1,
                   FUN = function(x) length(unique(x)) == 1)


  # return those strings but just from the start
  equal_start_tokens <- as.logical(cummin(overlap))
  out <- paste(x_tokenized[[1]][equal_start_tokens],
               collapse = "")

  return(out)
}

