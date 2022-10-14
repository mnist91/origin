#' Searches R Scripts for (Potential) Missings
#'
#' @param script script to originize
#' @param file file name of the script
#' @param functions list of function names to check
#' @param pkgs packages to consider
#' @template verbose
#' @template use_markers
#'
#' @return list of information where which package is (potentially) missing
#' @noRd
originize <- function(dat,
                      functions,
                      pkgs = getOption("origin.pkgs", .packages()),
                      verbose = FALSE,
                      use_markers =
                        getOption("origin.use_markers_for_logging")) {
  
  # data.frame of function-package pairs
  dat_fctns <- Reduce(f = rbind,
                      x = Map(f = function(pkg, fcts) {
                        data.frame(pkg = paste0(pkg, "::"),
                                   fct = fcts,
                                   pkg_nchar = nchar(pkg) + 2L)},
                        pkgs,
                        functions))
  
  # set an id column to sort the data into its original order after joins
  dat$Id <- seq_len(nrow(dat))
  
  # find function calls in scripts
  dat_fctns$MERGE_HELPER <- "FUNCTION_CALL"
  dat <- merge.data.frame(x = dat,
                          y = dat_fctns,
                          by.x = c("text", "usage"),
                          by.y = c("fct", "MERGE_HELPER"),
                          all.x = TRUE,
                          sort = FALSE,
  )
  
  # sort it in the original order
  dat <- dat[order(dat$Id), ]
  
  add_pkg <- !is.na(dat$pkg_nchar)
  # cases, for which no corresponding package has been found set added string
  # length to 0
  dat$pkg_nchar[!add_pkg] <- 0
  
  
  dat_out <- dat
  dat_out$text[add_pkg] <- paste0(dat_out$pkg[add_pkg],
                                  dat_out$text[add_pkg])
  
  fix_column_values <- function(dat) {
    # calculate, how many characters must be added to the line.
    # necessary to return a white space preserving result
    dat$nchar_csum <- unlist(
      # using tapply() as it is used in `by()`. This skips unneeded steps like
      # converting the vector into a data.frame
      tapply(X = seq_along(dat$pkg_nchar), 
             INDEX = list(dat$line1, dat$file), 
             FUN = function(x) cumsum(dat$pkg_nchar[x])))
    
    dat$col1 <- dat$col1 + dat$nchar_csum - dat$pkg_nchar
    dat$col2 <- dat$col2 + dat$nchar_csum
    
    return(dat)
  }
  
  dat_out <- fix_column_values(dat_out)
  
  files <- unique(dat_out$file)
  result <- stats::setNames(
    lapply(files, 
           function(f) revert_parse_data(dat_out[dat_out$file == f, ])),
    files)
  
  
  
  
  # if no logging is desired, skip all relevant steps
  if (!verbose) {
    return(list(to_write = result,
                logging_data = any(!is.na(dat_out$pkg))))
    
  } else {
    dat_logging <- dat
    
    dat_logging$log_type <- ""
    
    dat_logging[!is.na(dat_logging$pkg), "log_type"] <- "INSERT"
    
    # lines with function names that are not used as function names
    dat_logging[dat_logging$text %in% dat_fctns$fct &
                  is.na(dat_logging$pkg) &
                  dat_logging$token == "SYMBOL", "log_type"] <- "MISSING"
    
    # lines with function names that are not used as function names
    dat_logging[dat_logging$text %in% dat_fctns$fct &
                  is.na(dat_logging$pkg) &
                  dat_logging$token == "SPECIAL", "log_type"] <- "SPECIAL"
    
    # lines that are relevant for logging
    rel_logging <- unique(dat_logging[nzchar(dat_logging$log_type),
                                      c("line1", "file")])
    nrow(dat_logging)
    dat_logging <- merge.data.frame(rel_logging,
                                    dat_logging,
                                    by = c("line1", "file"))
    nrow(dat_logging)
    dat_logging <- dat_logging[order(dat_logging$Id), ]
    
    
    # in case there are lines with html-characters which might intervene with
    # the markers logging output eventually, escape them
    if (use_markers &&
        (length(has_html_token <- which(grepl(pattern = "<|>", 
                                              x = dat_logging$text))) > 0)) {
      
      # tokens with html characters to escape
      html_txt <- dat_logging$text[has_html_token]
      
      # count added characters to each token to recover white spaces later
      dat_logging$pkg_nchar[has_html_token] <- 
        dat_logging$pkg_nchar[has_html_token] + 
        # each escaped <> takes 3 additional characters of space
        lengths(regmatches(html_txt, gregexpr("<|>", html_txt))) * 3
      
      # replace escapable html characters
      dat_logging$text[has_html_token]  <-
        gsub(pattern     = ">",
             replacement = "&gt;",
             x           = gsub(pattern     = "<",
                                replacement = "&lt;",
                                x           = html_txt))
    }
    
    # html strings to wrap highlighted areas
    if (use_markers) {
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
    
    add_color <- function(dat, type, start_string, end_string) {
      
      # no occasions for this log type
      if (!any(rel_tokens <- dat$log_type == type)) {
        return(dat)
      }
      
      if (type == "INSERT") {
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
    
    dat_logging <- add_color(dat_logging, 
                             type = "INSERT", 
                             start_string = ins_start_string, 
                             end_string = end_string)
    dat_logging <- add_color(dat_logging, 
                             type = "MISSING", 
                             start_string = mis_start_string, 
                             end_string = end_string)
    dat_logging <- add_color(dat_logging, 
                             type = "SPECIAL", 
                             start_string = spe_start_string, 
                             end_string = end_string)
    
    # TODO. data structure
    xdat <- fix_column_values(dat_logging)
    
    xcol2 <- c(0, xdat$col2[-nrow(xdat)])
    new_line <- which(xdat$line1[-1] != xdat$line1[-nrow(xdat)] |
                        xdat$file[-1] != xdat$file[-nrow(xdat)]) + 1
    new_line <- c(1, new_line)
    xcol2[new_line] <- 0
    n_ws <- xdat$col1 - xcol2 - 1
    ws <- strrep(" ", n_ws)
    
    # add whitespaces to the tokens
    xdat$text <- paste0(ws, xdat$text)
    xdat
    
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
    

    return(list(to_write = result,
                logging_data = logging_data))
  }
  
}
