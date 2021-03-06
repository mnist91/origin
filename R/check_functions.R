#' Which functions are potentially used in the script
#'
#' This is a fast check to extract potentially used functions. It only checks if
#' the function name strings are present in any way. This reduces the number
#' of functions that must be considered more closely significantly. It speeds
#' up all further steps
#'
#' @param script a script to check
#' @param functions a vector with function names
#' @param ignore_comments a boolean, if TRUE lines starting with # are ignored
#' @param pkg package name from which the functions stem from
#' @template verbose
#'
#' @return
#' @noRd
#'
check_functions <- function(script,
                            functions,
                            ignore_comments = TRUE,
                            pkg = NULL,
                            verbose = TRUE) {
  # remove function with infix characters like %, &, [] and :
  infix_function_tokens <- c("*", "%", "?", "^", "$", "(", ")", "[",
                               "]", "{", "}", ":", "=", "<", ">")
  infix_functions <- grepl(functions,
                             pattern = paste0("[",
                                             paste(escape_strings(infix_function_tokens),
                                                   collapse = "|"),
                                             "]"
                                             ))
  relevant_functions <- functions[!infix_functions]
  # reduce the number of functions to check, by selecting possible (occurring)
  # functions. This isn't a check if it is a function or an object,
  # but a simple regular expression

  full_script <- paste0(script, collapse = "")

  matches <- vapply(X = relevant_functions,
                    FUN = function(fun) {
                      grepl(pattern = fun,
                            x = full_script,
                            fixed = TRUE)
                    },
                    FUN.VALUE = logical(1))
  functions_in_script <- relevant_functions[matches]

  # infix functions such as %like" can not be called with ::
  # print a warning, that such functions occur
  infix_matches <- vapply(X = functions[infix_functions],
                            FUN = function(fun) {
                              grepl(pattern = fun,
                                    x = full_script,
                                    fixed = TRUE)
                            },
                            FUN.VALUE = logical(1))

  # no matching functions
  if (!any(matches) && !any(infix_matches)) {
    return(NULL)
  }

  # reduce the number of script rows to check, by selecting only those which
  # contain a function name
  if (any(matches)) {
    line_matches <- grepl(
      x = script,
      pattern = paste(escape_strings(functions_in_script), collapse = "|"))
  } else {
    line_matches <- rep(FALSE, length(script))
  }

  # ignore comment rows
  if (ignore_comments) {
    line_comments <- grepl(x = trimws(script[line_matches]), pattern = "^#")

    # ignore these line for the matching
    line_comments_matches <- which(line_matches)[which(line_comments)]
    line_matches[line_comments_matches] <- FALSE

  }


  return(list(matches = matches,
              line_matches = line_matches,
              infix_matches = infix_matches,
              infix_functions = infix_functions,
              functions_in_script = functions_in_script))

}
