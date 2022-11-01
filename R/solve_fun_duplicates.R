#' print warning regarding conflicts
#' User input is required when called in an interactive context, e.g. RStudio
#' @param dups duplicate functions
#' @param pkgs vector of checked packages to determine order
#' @template ask_before_applying_changes
#'
#' @return No return value, called for side effects
#'
#' @noRd
solve_fun_duplicates <- function(dups, 
                                 pkgs,
                                 ask_before_applying_changes = TRUE) {
  # Require User interaction if duplicates are detected
  
  if (interactive() && ask_before_applying_changes) {
    # nocov start
    # bold, red and underlined text
    cat(paste0("\033[31m\033[4m\033[1m",
               "Used functions in mutliple Packages!",
               "\033[22m\033[24m\033[39m",
               "\n\n"))
    
    cat(paste(dups, ": ", names(dups),
              collapse = "\n", sep = ""),
        "\n\n")
    cat("Order in which relevant packges are evaluated:\n")
    dup_nms <- unique(unlist(strsplit(names(dups), ", ")))
    cat(paste(pkgs[pkgs %in% dup_nms], collapse = " >> "), "\n")
    
    cat("Do you want to proceed?")
    answer <- utils::menu(choices = c("YES", "NO"))
    # nocov end
  } else {
    answer <- 1
  }
  if (answer != 1) {
    stop("Execution halted") # nocov
  }
  
  return(invisible(NULL))
  
}
