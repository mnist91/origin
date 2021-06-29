# assign new script and ask for user permission if required
apply_changes <- function(ask_before_applying_changes,
                          result,
                          init_script,
                          type,
                          context = NULL) {

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

    # nocov start
    if (ask_before_applying_changes && interactive()) {
      cat("\nHappy with the result? \U0001f600\n\n")
      answer <- utils::menu(choices = c("YES", "NO"))
      if (answer != 1) {
        message("No changes made!")
        return(invisible(NULL))
      }
    }
    # nocov end

    if (type == "writeLines") {
      # extract non-empty new scripts
    new_scripts <- Filter(result, f = function(l) !is.null(l$to_write$script))
    # assign scripts where changes are made
    lapply(X = new_scripts,
           FUN = function(x) writeLines(text = x$to_write$script,
                                        con = x$to_write$file))

    return(invisible(NULL))



    # return plane text
    } else if (type == "paste") {
      script_out <- result[[1]]$to_write$script

      # replace character(0) by "" for more consistent testing
      script_out <- rapply(object = script_out,
                           f = function(x) {
                             ifelse(length(x) == 0, "", x)
                           },
                           how = "replace")
      return(paste(script_out, collapse = "\n"))





      # insert Text via apistudioapi
    } else if (type == "insertText") {
      to_insert <- paste(result[[1]]$to_write$script, collapse = "\n")

      selected_range <- context$selection[1][[1]]$range

      # if end of selection is at beginning of a new line, extra line
      # break is required to keep the same document structure
      if (selected_range$end[2] == 1) {
        to_insert <- paste0(to_insert, "\n")
      }

      rstudioapi::insertText(text = to_insert,
                             location = context$selection[1][[1]]$range,
                             id = context$id)
      return(invisible(NULL))

    }

  }
}
