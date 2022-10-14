#' Add Package Name to Function Calls
#'
#' @param pkg a package name
#' @param script character vector of script lines
#' @param functions a vector with function names
#'
#' @noRd
#' @return list that contains all information to where insert which package
get_origins <- function(pkg,
                        script,
                        functions = NULL,
                        specific = FALSE,
                        infix_functions = NULL) {


  # get all used functions in the script --------------------------------
  l <- check_functions(script = script,
                       functions = functions,
                       pkg = pkg)

  # make function-Output available
  matches <- l$matches
  line_matches <- l$line_matches
  functions_in_script <- l$functions_in_script
  if (!any(matches)) {
    return(list())
  }


  funs_prep <- paste0(escape_strings(functions_in_script), collapse = "|")

  # tokens that can occur right before a function calls
  # Begin Exclude Linting
  # pre_fun_tokens <- c(",", ";", "=", "&", "/", "-", "<", ">", "~", "!", "|",
  #                     "?", "*", "^", "+", "(", "[", "{")
  # paste(escape_strings(pre_fun_tokens), collapse = "")
  # End Exclude Linting
  pattern_regex <-
    paste0("(?<=[[:blank:],;=&/\\-<>~!\\|\\?\\*\\^\\+\\(\\[\\{]|^)(",
           funs_prep,
           ") *\\(")

  regular_calls <- get_matches(script[line_matches],
                               line = which(line_matches),
                               regex = pattern_regex,
                               perl = TRUE,
                               filter_nomatches = FALSE)
  # In functional programming, the function's call is used without
  # brackets but as an object. To differentiate between such a
  # function and a variable, an explicit parameter is needed:
  # e.g. *apply "FUN = "
  # e.g. purr ".f = "
  # regular expression, to identify usage with *apply/purrr
  fun_arguments <- c("\\WFUN", "\\W\\.f")
  assign_options <- c("=", " = ", "= ", " =")
  leading_patterns <- paste(apply(expand.grid(fun_arguments, assign_options),
                                  MARGIN = 1,
                                  FUN = paste0,
                                  collapse = ""),
                            collapse = "|")
  functional_pattern_regex <-
    paste0("(?<=", leading_patterns, ")(",
           funs_prep, ")(?=[ \\,)]|$)")
  functional_calls <- get_matches(script[line_matches],
                                  line = which(line_matches),
                                  regex = functional_pattern_regex,
                                  perl = TRUE,
                                  fixed = FALSE,
                                  filter_nomatches = FALSE)
  if (specific) {

    specific_pattern_regex <- 
      paste0("(", pkg, "\\s*\\:\\:+\\s*(", funs_prep, "))")
    specific_calls <- get_matches(script[line_matches],
                                  line = which(line_matches),
                                  regex = specific_pattern_regex,
                                  perl = TRUE,
                                  filter_nomatches = FALSE)

    if (length(infix_functions) > 0) {
      infix_funs_prep <- 
        paste0(escape_strings(infix_functions), collapse = "|")
      infix_fun_regex <- paste0("(?<=\\W)(", infix_funs_prep, ")(?=\\W)")
      infix_calls <- get_matches(script[line_matches],
                                 line = which(line_matches),
                                 regex = infix_fun_regex,
                                 perl = TRUE,
                                 filter_nomatches = FALSE)
      matches <- Map(comb_matches,
                     functional_calls$matches,
                     regular_calls$matches,
                     specific_calls$matches,
                     infix_calls$matches)
      match_length <- Map(comb_matches,
                          functional_calls$log_length,
                          regular_calls$log_length,
                          specific_calls$log_length,
                          infix_calls$log_length)
    } else {
      matches <- Map(f = comb_matches,
                     functional_calls$matches,
                     regular_calls$matches,
                     specific_calls$matches)
      match_length <- Map(f = comb_matches,
                          functional_calls$log_length,
                          regular_calls$log_length,
                          specific_calls$log_length)

    }



  } else {
    matches <- Map(comb_matches,
                   functional_calls$matches,
                   regular_calls$matches)
    match_length <- Map(comb_matches,
                        functional_calls$log_length,
                        regular_calls$log_length)
  }


  has_match <- lapply(matches, length) != 0

  n_matches <- sum(has_match)
  orig_list <- list(line = regular_calls$line[has_match],
                    string = regular_calls$string[has_match],
                    matches = matches[has_match],
                    match_length = match_length[has_match],
                    log_length = lapply(matches[has_match],
                                        FUN = function(x) rep(0, length(x))),
                    pkg = rep(paste0(pkg, "::"), n_matches),
                    type = rep("INSERT", n_matches))

  return(orig_list)
}


# keep results of greprex where a match has been found
comb_matches <- function(...) {
  x <- c(...) # Exclude Linting
  return(x[x != -1])
}
