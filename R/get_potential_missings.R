#' Find Potentially Missed Exported Functions
#'
#' @param script the script before any changes
#' @param line_matches a boolean vector with the lines that contain changes
#' @param functions a vector with function names
#' @param functions_in_script a vector which functions were used
#' @param infix_functions a vector with infix functions such as `\%like\%`
#' @param infix_matches a boolean vector that indicates which infix
#'   functions are used
#'
#' @return list of potential missed functions and their position.
#' @noRd
get_potential_missings <- function(script,
                                   line_matches,
                                   functions,
                                   functions_in_script,
                                   infix_functions = NULL,
                                   infix_matches = FALSE) {

  # lines where a function name occurred, but no changed happened
  # not comprehensive, since there might be line where one function was
  # recognized, but not another
  potential_missings <- script[line_matches]

  # check for functions
  # infix regex characters in functions like dots must be escaped
  # function names  should not be preceded by a double colon OR character nor
  # succeeded by a double colon OR a percentage sign OR a character
  funs_prep <- paste(escape_strings(functions_in_script), collapse = "|")

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

  # did infix functions such as "%like" or %>% which are not used with
  # PACAKGE::FUNCTION occur
  if (any(infix_matches)) {

    infix_functions_in_script <- functions[infix_functions][infix_matches]

    # lines with infix functions
    infix_matches <- which(as.logical(
      Reduce(f = "+", lapply(X = infix_functions_in_script,
                             FUN = function(pattern) grepl(x = script,
                                                           pattern = pattern,
                                                           fixed = TRUE)))
    ))

    funs_prep <- paste(escape_strings(infix_functions_in_script),
                       collapse = "|")

    list_infixes <- get_matches(line = infix_matches,
                                 text = script[infix_matches],
                                 regex = funs_prep,
                                 perl = FALSE,
                                 fixed = FALSE,
                                 filter_nomatches = TRUE)
    n_infixes <-  length(list_infixes$line)
    list_infixes <- c(list_infixes,
                       list(pkg = rep("", n_infixes),
                            type = rep("infix", n_infixes)
                       ))
  } else {
    list_infixes <- NULL
  }

  out <- list(infixes = list_infixes, pot_missings = list_pot_missings)
  return(out)
}
