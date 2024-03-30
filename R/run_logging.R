#' Invoke Logging Either Via Markers Pane or Console Output
#'
#' @param dat list of relevant information
#' @param use_markers logical, whether to use the markers pane or console output
#'
#' @return No return value, called for side effects
#' @noRd
run_logging <- function(dat, use_markers) {

  if (length(dat$line) == 0) {
    message("No functions detected in scripts")
    return(invisible(NULL))
  }

  # invoke markers tabs
  if (use_markers) {
    rstudioapi::sourceMarkers(name = "origin",
                              markers = dat)

    # console logging output
  } else {

    # iterate over each file
    lapply(unique(dat$file), function(x) {
      sub_dat <- dat[dat$file == x, ]

      # print file information first and all logging observations
      # for this file below
      cat(paste("\n", sub_dat$file[1],
                paste(sub_dat$type, " ",
                      sub_dat$line, ": ",
                      sub_dat$message,
                      collapse = "\n"),
                sep = "\n"))
    })
  }

  return(invisible(NULL))
}
