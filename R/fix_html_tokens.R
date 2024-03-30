# for the markers output, escape html specific tokens
fix_html_tokens <- function(dat) {
  has_html_token <- which(grepl(pattern = "<|>",
                                x = dat$text))

  # in case special characters that intervene with html code are present
  # replace them by their html counterpart
  if (length(has_html_token) > 0) {

    # tokens with html characters to escape
    html_txt <- dat$text[has_html_token]

    # count added characters to each token to recover white spaces later
    dat$pkg_nchar[has_html_token] <-
      dat$pkg_nchar[has_html_token] +
      # each escaped <> takes 3 additional characters of space
      lengths(regmatches(html_txt, gregexpr("<|>", html_txt))) * 3

    # replace escapable html characters
    dat$text[has_html_token]  <-
      gsub(pattern     = ">",
           replacement = "&gt;",
           x           = gsub(pattern     = "<",
                              replacement = "&lt;",
                              x           = html_txt))
  }

  return(dat)
}

# recapturing the original code structure after insertions have been made
fix_column_values <- function(dat) {
  grps <- paste(dat$line1, dat$file)
  # using tapply() as it is used in `by()`. This skips unneeded steps like
  # converting the vector into a data.frame
  cumsums <- tapply(X     = dat$pkg_nchar,
                    INDEX = grps,
                    # calculate, how many characters must be added to the line.
                    # necessary to return a white space preserving result
                    FUN   = cumsum,
                    simplify = FALSE)
  # NOTE: tapply does not retain the order of the array!
  dat$nchar_csum <- unlist(cumsums[unique(grps)])

  dat$col1 <- dat$col1 + dat$nchar_csum - dat$pkg_nchar
  dat$col2 <- dat$col2 + dat$nchar_csum

  return(dat)
}

# add color highlighting for specific strings
add_color <- function(dat, type, start_string, end_string,
                      type_fun = "originize") {

  # no occasions for this log type
  if (!any(rel_tokens <- dat$log_type == type)) {
    return(dat)
  }

  if (type_fun == "originize" && type == "INSERT") {
    dat[rel_tokens, "text"] <-
      # only hoghlight inserted package name
      paste0(start_string, dat[rel_tokens, "pkg"], end_string,
             dat[rel_tokens, "text"])

  } else {
    dat[rel_tokens, "text"] <-
      # highlight original token
      paste0(start_string, dat[rel_tokens, "text"], end_string)
  }

  # how many characters were added
  # relevant for reverting data
  dat[rel_tokens, "pkg_nchar"] <-
    dat[rel_tokens, "pkg_nchar"] + nchar(start_string) + nchar(end_string)

  return(dat)
}

# geenrate logging data from input data
make_logging_data <- function(dat, use_markers, type_fun) {
  # lines that are relevant for logging
  rel_logging <- unique(dat[nzchar(dat$log_type),
                            c("line1", "file")])

  dat_logging <- merge.data.frame(rel_logging,
                                  dat,
                                  by = c("line1", "file"))

  dat_logging <- dat_logging[order(dat_logging$Id), ]


  # in case there are lines with html-characters which might intervene with
  # the markers logging output eventually, escape them
  if (use_markers) {
    dat_logging <- fix_html_tokens(dat_logging)
  }

  # html strings to wrap highlighted areas
  # html format is not supported anymore:
  # see: https://github.com/rstudio/rstudio/issues/10062#issuecomment-1044883002
  if (use_markers & FALSE) {
    ins_start_string <- sprintf('<text style="color: %s;">',
                                getOption("origin.color_added_package"))
    mis_start_string <-  sprintf('<text style="color: %s;">',
                                 getOption("origin.color_missed_function"))
    spe_start_string <-  sprintf('<text style="color: %s;">',
                                 getOption("origin.color_infix_function"))
    end_string <- "</text>"

    # bash colors for colored console text
    # mimics behavior of crayon
  } else {
    # TODO: color codes depending on theme
    ins_start_string <- "\033[36m"
    mis_start_string <- "\033[31m"
    spe_start_string <- "\033[33m"
    end_string <- "\033[39m"
  }

  dat_logging <- add_color(dat_logging,
                           type_fun = type_fun,
                           type = "INSERT",
                           start_string = ins_start_string,
                           end_string = end_string)
  dat_logging <- add_color(dat_logging,
                           type_fun = type_fun,
                           type = "MISSING",
                           start_string = mis_start_string,
                           end_string = end_string)
  dat_logging <- add_color(dat_logging,
                           type_fun = type_fun,
                           type = "SPECIAL",
                           start_string = spe_start_string,
                           end_string = end_string)

  # fix data structure
  xdat <- fix_column_values(dat_logging)

  # calcualte needed whitespaces
  xcol2 <- c(0, xdat$col2[-nrow(xdat)])
  new_line <- which(xdat$line1[-1] != xdat$line1[-nrow(xdat)] |
                      xdat$file[-1] != xdat$file[-nrow(xdat)]) + 1
  new_line <- c(1, new_line)
  xcol2[new_line] <- 0
  n_ws <- xdat$col1 - xcol2 - 1
  ws <- strrep(" ", n_ws)

  # add whitespaces to the tokens
  xdat$text <- paste0(ws, xdat$text)

  log_dat <- by(data = xdat,
                INDICES = list(xdat$line1, xdat$file),
                FUN = function(xd) {
                  data.frame(file = xd$file[1],
                             line = xd$line1[1],
                             column = xd$col1[which.min(xd$log_type != "")],
                             type = set_marker_type(xd$log_type),
                             message = paste(xd$text, collapse = ""))
                }
  )

  logging_data <- Reduce(f = rbind, x = log_dat)


  if (use_markers && !is.null(logging_data) && nrow(logging_data) > 0) {
    attr(logging_data$message, which = "class") <- c("html", "character")
  }

  return(logging_data)
}
