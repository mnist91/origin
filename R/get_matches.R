#' Find String Matches and Store Information About Them
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

