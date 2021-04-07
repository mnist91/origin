# assign new script and ask for user permission if required
apply_changes <- function(ask_before_applying_changes, result, init_script) {

  # did all scripts remain the same
  all_unchanged <- all(mapply(function(x, y) isTRUE(all.equal(x, y)),
                              init_script,
                              lapply(result, function(l) l$to_write$script),
                              SIMPLIFY = TRUE))

  # in case they are, notify the user
  if (all_unchanged) {
    message("No unspecified functions detected. Script remains as is.")
    return(invisible(NULL))


    # if changes were made, ask user if those can be overwrite the files
  } else {

    if (ask_before_applying_changes && interactive()) {
      cat("\nHappy with the result? \U0001f600\n\n")
      answer <- menu(choices = c("YES", "NO"))
      if (answer != 1) {
        message("No changes made!")
        return(invisible(NULL))
      }
    }

    # extract non-empty new scripts
    new_scripts <- Filter(result, f = function(l) !is.null(l$to_write$script))
    # assign scripts where changes are made
    lapply(X = new_scripts,
           FUN = function(x) writeLines(text = x$to_write$script,
                                        con = x$to_write$file))

    return(invisible(NULL))
  }
}
