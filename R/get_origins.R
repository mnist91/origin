#' Add Package Name to Function Calls
#'
#' @param pkg a package name
#' @param file a path to a script
#' @param overwrite a boolean, if TRUE the file will be saved and overwritten. If FALSE the file is returned.
#' @param ignoreComments a boolean, if TRUE lines starting with # are ignored
#' @param verbose a boolean
#' @param functions a vector with function names
#'
#' @noRd
#' @return

get_origins <- function(pkg,
                        script,
                        file = NULL,
                        overwrite = FALSE,
                        ignoreComments = TRUE,
                        verbose = FALSE,
                        functions = NULL) {
  
  
  # get all exported functions from a package --------------------------------
  if (is.null(functions)) {
    functions <- getFunctions(pkg)
  }
  
  l <- checkFunctions(script = script,
                      functions = functions,
                      pkg = pkg,
                      verbose = verbose)
  
  # make function-Output available
  matches <- l$matches
  line_matches <- l$line_matches
  special_matches <- l$special_matches
  functions_in_script <- l$functions_in_script
  special_functions <- l$special_functions
  if (!any(matches)) {
    return(list())
  }


  funs_comb <- paste0(functions_in_script, collapse = "|")
  funs_prep <-  gsub("\\.", "\\\\.", x = funs_comb)
  # tokens that can occur right before a function calls
  pattern_regex <- paste0("(?<=[[:blank:],;=&/\\-<>~\\!\\?\\*\\^\\+\\(\\[]|^)(", funs_prep, ") *\\(")
  
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
  funArguments <- c("FUN", "\\.f")
  assignWithSpaces <- c("=", " = ", "= ", " =")
  leadingPatterns <- paste(apply(expand.grid(funArguments, assignWithSpaces),
                                 MARGIN = 1,
                                 FUN = paste0,
                                 collapse = ""),
                           collapse = "|")
  functionalPatternRegex <-
    paste0("(?<=", leadingPatterns, ")(",
           funs_prep, ")(?=[ \\,)]|$)")
  functional_calls <- get_matches(script[line_matches],
                                  line = which(line_matches),
                                  regex = functionalPatternRegex,
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
                    log_length = lapply(matches[has_match], function(x) rep(0, length(x))),
                    pkg = rep(paste0(pkg, "::"), n_matches),
                    type = rep("insert", n_matches))
  
  return(orig_list)
}
