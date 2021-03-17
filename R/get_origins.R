#' Add Package Name to Function Calls
#'
#' @param pkg a package name
#' @param script character vector of script lines
#' @template file 
#' @template overwrite
#' @template ignore_comments
#' @template verbose
#' @param functions a vector with function names
#'
#' @noRd
#' @return list that contains all information to where insert which package
get_origins <- function(pkg,
                        script,
                        file = NULL,
                        overwrite = FALSE,
                        ignore_comments = TRUE,
                        verbose = FALSE,
                        functions = NULL) {


  # get all exported functions from a package --------------------------------
  if (is.null(functions)) {
    functions <- get_exported_functions(pkg)
  }

  l <- check_functions(script = script,
                      functions = functions,
                      pkg = pkg,
                      verbose = verbose)

  # make function-Output available
  matches <- l$matches
  line_matches <- l$line_matches
  functions_in_script <- l$functions_in_script
  if (!any(matches)) {
    return(list())
  }


  funs_comb <- paste0(functions_in_script, collapse = "|")
  funs_prep <-  gsub("\\.", "\\\\.", x = funs_comb)
  # tokens that can occur right before a function calls
  pattern_regex <- paste0("(?<=[[:blank:],;=&/\\-<>~\\!\\?\\*\\^\\+\\(\\[]|^)(",
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
  fun_arguments <- c("FUN", "\\.f")
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

  matches <- Map(comb_matches,
                 functional_calls$matches,
                 regular_calls$matches)

  has_match <- lapply(matches, length) != 0

  n_matches <- sum(has_match)
  orig_list <- list(line = regular_calls$line[has_match],
                    string = regular_calls$string[has_match],
                    matches = matches[has_match],
                    # log_length = Map(comb_matches,
                    #                  functional_calls$match_length,
                    #                  regular_calls$match_length)[has_match],
                    log_length = lapply(matches[has_match],
                                        FUN = function(x) rep(0, length(x))),
                    pkg = rep(paste0(pkg, "::"), n_matches),
                    type = rep("insert", n_matches))

  return(orig_list)
}
