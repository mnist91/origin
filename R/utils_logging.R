#' Print Change Log and potential Errors in Console
#'
#' @param script the script before any changes
#' @param line_matches a boolean vector with the lines that contain changes
#' @param functions a vector with function names
#' @param functions_in_script a vector which functions were used
#' @param special_functions a vector with special functions such as `\%like\%`
#' @param special_matches a boolean vector that indicates which special
#'   functions are used
#'
#' @return
#' @noRd
prep_verbose <- function(script,
                         line_matches,
                         functions,
                         functions_in_script,
                         special_functions = NULL,
                         special_matches = FALSE) {
  
  # lines where a function name occurred, but no changed happened
  # not comprehensive, since there might be line where one function was
  # recognized, but not another
  potential_missings <- script[line_matches]
  
  # check for functions 
  # special regex characters in functions like dots must be escaped
  # function names  should not be preceded by a double colon OR character nor
  # succeeded by a double colon OR a percentage sign OR a character
  funs_comb <- paste(functions_in_script, collapse = "|")
  funs_prep <- gsub("\\.", "\\\\.", x = funs_comb)
  fun_regex <- paste0("(?<!::|[[:alnum:]])(",
                      funs_prep,
                      ")(?!::|%|[[:alnum:]])")
  
  
  list_pot_missings <- get_matches(line = which(line_matches),
                                   text = potential_missings,
                                   regex = fun_regex,
                                   perl = TRUE,
                                   fixed = FALSE,
                                   filter_nomatches = TRUE)
  n_potentials <- length(list_pot_missings$line)
  list_pot_missings <- c(list_pot_missings, 
                         list(pkg = rep("", n_potentials),
                              type = rep("missed", n_potentials)
                         ))
  
  # did special functions such as "%like" or %>% which are not used with
  # PACAKGE::FUNCTION occur
  if (any(special_matches)) {
    
    special_functions_in_script <- functions[special_functions][special_matches]
    
    # lines with special functions
    special_matches <- which(as.logical(
      Reduce(f = "+", lapply(X = special_functions_in_script,
                             FUN = function(pattern) grepl(x = script,
                                                           pattern = pattern,
                                                           fixed = TRUE)))
    ))
    
    funs_comb <- paste(special_functions_in_script, collapse = "|")
    
    # potential special characters that need do be escaped in regexes
    funs_prep <- gsub("\\.", "\\\\.", x = funs_comb)
    funs_prep <- gsub("\\%", "\\\\%", x = funs_prep)
    funs_prep <- gsub("\\[", "\\\\[", x = funs_prep)
    funs_prep <- gsub("\\]", "\\\\]", x = funs_prep)
    funs_prep <- gsub("\\$", "\\\\$", x = funs_prep)
    funs_prep <- gsub("<", "\\<", x = funs_prep)
    funs_prep <- gsub(">", "\\>", x = funs_prep)
    
    list_specials <- get_matches(line = special_matches,
                                 text = script[special_matches],
                                 regex = funs_prep,
                                 perl = FALSE,
                                 fixed = FALSE,
                                 filter_nomatches = TRUE)
    n_specials <-  length(list_specials$line)
    list_specials <- c(list_specials, 
                       list(pkg = rep("", n_specials),
                            type = rep("special", n_specials)
                       ))
  } else {
    list_specials <- NULL
  }
  
  out <- list(specials = list_specials, pot_missings = list_pot_missings)
  return(out)
}


#' find potential missings in the data and store information
#' about type of missing, length of match and line
#'
#' @param text character vector of relevant script
#' @param line line information to each text element
#' @param regex a regular expression, containing all function names in OR (pipe)
#'   combinations.
#' @param perl whether perl regular expressions should be used
#' @param fixed whether the regex part is evaluated as is
#' @param filter_nomatches whether to not return lines without a match
#'
#' @return list of match information, i.e. which line, text, position, and
#'    match length
#' @noRd
get_matches <- function(text,
                        line,
                        regex,
                        perl = TRUE,
                        fixed = FALSE,
                        filter_nomatches = TRUE) {
  matches <- gregexpr(pattern = regex,
                      text = text,
                      perl = perl,
                      fixed = fixed)
  
  if (filter_nomatches) {
    has_matches <- vapply(X = matches,
                          FUN = function(x) any(x != -1),
                          FUN.VALUE = logical(1))
    
    matches <- matches[has_matches]
    
    out <- list(line = line[has_matches],
                string = text[has_matches],
                matches = lapply(matches,
                                 FUN = as.numeric),
                log_length = lapply(matches,
                                    FUN = attr,
                                    which = "match.length"))
  } else {
    out <- list(line = line,
                string = text,
                matches = lapply(matches,
                                 FUN = as.numeric),
                log_length = lapply(matches,
                                    FUN = attr,
                                    which = "match.length"))
    
  }
  
  return(out)
  
}




# print warning regarding conflicts
#' @importFrom utils menu
solve_fun_duplicates <- function(dups, pkgs) {
  # Require User interaction if duplicates are detected
  
  # bold, red and underlined text
  cat("\033[31m\033[4m\033[1m",
      "Used functions in mutliple Packages!",
      "\033[22m\033[24m\033[39m",
      "\n")
  dups_with_package <- by(names(dups), dups, paste, collapse = ", ")
  
  cat(paste(dups_with_package, ": ", names(dups_with_package),
            collapse = "\n", sep = ""),
      "\n")
  cat("Order in which relevant packges are evaluated;\n\n")
  cat(paste(pkgs[pkgs %in% names(dups)], collapse = " >> "), "\n")
  
  cat("Do you want to proceed?\n")
  if (interactive()) {
    answer <- menu(choices = c("YES", "NO"))
  } else {
    answer <- 1
  }
  if (answer != 1) {
    stop("Execution halted")
  }
  
}

