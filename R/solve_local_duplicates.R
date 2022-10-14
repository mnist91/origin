#' print warning regarding conflicts
#' User input is required when called in an interactive context, e.g. RStudio
#' @param dups duplicate functions
#'
#' @return No return value, called for side effects
#'
#' @noRd
solve_local_duplicates <- function(dups) {
  # Require User interaction if duplicates are detected

  # bold, red and underlined text
  cat(
    "\033[31m\033[4m\033[1m",
    "Locally defined and used functions mask exported functions from packages!",
    "\033[22m\033[24m\033[39m",
    "\n\n")

  cat(paste(dups, ": ", names(dups),
            collapse = "\n", sep = ""),
      "\n\n")
  cat("Local functions have \033[4mhigher\033[24m priority. In case you want to use an", # Exclude Linting
      "exported version of a function listed above set pkg::fun manually\n\n")

  cat("Got it?")
  if (interactive()) {
    answer <- utils::menu(choices = c("YES", "NO")) # nocov
  } else {
    answer <- 1
  }
  if (answer != 1) {
    stop("Execution halted") # nocov
  }

  return(invisible(NULL))

}
