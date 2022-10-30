#' assign new script and ask for user permission if required
#'
#' @template ask_before_applying_changes
#' @param result list comming from `originize`
#' @param init_script character vector of initial script lines
#' @param type whether to `writeLines` in a file, `insertText` at selection, or
#'   `paste` the string as is
#' @param context RStudio context of selected text
#'
#' @return either `NULL` if writing to a file or the originized file if
#'  overwrite selected/highlighted text.
#' @noRd
#' @importFrom utils menu
apply_changes <- function(ask_before_applying_changes,
                          result,
                          init_script,
                          type,
                          context = NULL) {

  # did all scripts remain the same
  unchanged <- mapply(function(x, y) isTRUE(all.equal(x, y)),
                      init_script,
                      result$to_write,
                      SIMPLIFY = TRUE)

  # in case they are, notify the user
  if (all(unchanged)) {
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
      new_scripts <- result$to_write[!unchanged]
      # assign scripts where changes are made
      mapply(new_scripts,
             names(new_scripts),
             FUN = function(x, f) {
               # NOTE: EOL-Issue:
               # If a file does not end in an EOL mark (LF or CRLF),
               # this often indicates a broken text file.
               # readLines() does not read in the last line but triggers a
               # warning of a file does not end in a EOL mark. 
               # Instead of using tryCatch to use these warnings to 
               # determine if a final line existed, all returned scripts 
               # will end with a final line which is
               # consistent with git. x[length(x) + 1] <- ""

               writeLines(text = x,
                          con = f)})

      return(invisible(NULL))



      # return plane text
    } else if (type == "paste") {
      script_out <- character(length(init_script[[1]]))
      script_out[seq_along(result$to_write[[1]])] <-
        result$to_write[[1]]

      return(paste(script_out, collapse = "\n"))





      # insert Text via rstudioapi
    } else if (type == "insertText") {
      to_insert <- character(length(init_script[[1]]))
      to_insert[which(lapply(init_script[[1]], length) == 1)] <-
        result$to_write[[1]]
      to_insert <- paste(to_insert, collapse = "\n")

      selected_range <- context$selection[1][[1]]$range

      # nocov start
      # if end of selection is at beginning of a new line, extra line
      # break is required to keep the same document structure
      if (!is.null(selected_range)) {
        if (selected_range$end[2] == 1) {
          to_insert <- paste0(to_insert, "\n")
        }

        rstudioapi::insertText(text = to_insert,
                               location = context$selection[1][[1]]$range,
                               id = context$id)
      }
      # nocov end
      return(invisible(NULL))

    }

  }
}
