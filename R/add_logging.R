# style a string for logging output
# TODO: colors depending on background /theme
add_logging <- function(string, splits, pkg, log_length, type, use_markers) {

  # html strings to wrap highlighted areas
  if (use_markers) {
    ins_start_string <- sprintf('<text style="color: %s;">',
                                getOption("origin.color_added_package"))
    mis_start_string <-  sprintf('<text style="color: %s;">',
                                 getOption("origin.color_missed_function"))
    spe_start_string <-  sprintf('<text style="color: %s;">',
                                 getOption("origin.color_infix_function"))
    end_string <- "</text>"
    start_wrapper <- "<div>"
    end_wrapper <- "</div"

    # bash colors for colored console text
    # mimics behavior of crayon
  } else {
    # TODO: color codes depending on theme
    ins_start_string <- "\033[36m"
    mis_start_string <- "\033[31m"
    spe_start_string <- "\033[33m"
    end_string <- "\033[39m"
    start_wrapper <- "\033[39m"
    end_wrapper <- "\033[39m"
  }

  # split the original line where color codes are to be inserted
  splitted <- substring(text = string,
                        first = c(1, splits),
                        last = c(splits - 1, nchar(string)))

  # which substrings must be highlighted
  to_highlight <- log_length != 0

  # include color highlighting, either html or bash by iterating over each split
  splitted[-1][to_highlight] <- as.character(
    Map(function(str, len, type) {
      to_log <- substring(text = str,
                          first = c(1, len + 1),
                          last = c(len, nchar(str)))
      if (type == "MISSING") {
        start_string <- mis_start_string
      } else {
        start_string <- spe_start_string
      }
      paste(start_string, to_log[1], end_string, to_log[2], sep = "")

    },
    splitted[-1][to_highlight],
    log_length[to_highlight],
    type[to_highlight])
  )

  # add all highlighted texts together
  to_insert <- log_length == 0
  splitted[-1][to_insert] <- paste(ins_start_string,
                                   pkg[to_insert], end_string,
                                   splitted[-1][to_insert],
                                   sep = "")

  # return complete string
  out <- paste0(start_wrapper,
                splitted[1],
                paste(splitted[-1],
                      collapse = ""),
                end_wrapper)

  return(out)
}
