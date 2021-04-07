#' Find Potentially Missed Exported Functions
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
get_potential_missings <- function(script,
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

  # do not consider string matches that are pre- or succeeded by a numeric,
  # character, doublecolon, underscore or dot
  fun_regex <- paste0("(?<!::|[[:alnum:]]|\\.|_|\\|)(",
                      funs_prep,
                      ")(?!::|%|[[:alnum:]]|\\.|_|\\|)")


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
