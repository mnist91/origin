# print warning regarding conflicts
# User input is required when called in an interactive context, e.g. RStudio
solve_fun_duplicates <- function(dups, pkgs) {
  # Require User interaction if duplicates are detected

  # bold, red and underlined text
  cat("\033[31m\033[4m\033[1m",
      "Used functions in mutliple Packages!",
      "\033[22m\033[24m\033[39m",
      "\n")
  dups_with_package <- by(names(dups), dups, paste, collapse = ", ")

  cat(paste(dups_with_package, ": ", names(dups_with_package),
            collapse = "\n", sep = ""),
      "\n")
  cat("Order in which relevant packges are evaluated;\n\n")
  cat(paste(pkgs[pkgs %in% names(dups)], collapse = " >> "), "\n")

  cat("Do you want to proceed?\n")
  if (interactive()) {
    answer <- menu(choices = c("YES", "NO"))
  } else {
    answer <- 1
  }
  if (answer != 1) {
    stop("Execution halted")
  }

  return(invisible(NULL))

}

