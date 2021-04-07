# invoke logging either via markers tab or console output
run_logging <- function(dat, use_markers) {

  if (length(dat$line) == 0) {
    message("No functions detected in script(s)")
    return(invisible(NULL))
  }

  # infoke markers tab
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
