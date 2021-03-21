# assign new script and ask for user permission if required
apply_changes <- function(ask_before_applying_changes, result) {
  if (ask_before_applying_changes && interactive()) {
    cat("\nHappy with the result? \U0001f600\n\n")
    answer <- menu(choices = c("YES", "NO"))
    if (answer != 1) {
      message("No changes made!")
      return(invisible(NULL))
    }
  }

  # assign scripts where changes are made
  lapply(X = Filter(result, f = function(l) !is.null(l$script)),
         FUN = function(x) writeLines(text = x$script, con = x$file))

  return(invisible(NULL))
}
