#' Print Change Log and potential Errors in Console
#'
#' @param script_prior the script before any changes
#' @param script_after the script after the changes
#' @param lineMatches a boolean vector with the lines that contain changes
#' @param functions a vector with function names
#' @param functionsInScript a vector which functions were used
#' @param special_functions a vector with special functions such as \%like\%
#' @param special_matches a boolean vector that indicates which special
#'   functions are used
#'
#' @return
#' @export
#'
verbolize <- function(script_prior,
                      script_after,
                      lineMatches = rep(TRUE, length(script_after)),
                      functions,
                      functionsInScript,
                      special_functions = NULL,
                      special_matches = FALSE) {
  # changed lines
  changes <- script_after[lineMatches] != script_prior[lineMatches]
  line_matches_pos <- which(lineMatches)

  # number of changed lines
  sumChanges <- sum(changes)
  cat(crayon::green(sumChanges, "Lines changed\n"))

  # list all recognized and used functions of a package
  cat(crayon::green(length(functionsInScript), "Functions recognized\n"))
  cat(paste(functionsInScript, collapse = "\n"), "\n")

  # list all changed lines
  cat(crayon::green("Changes:\n"))

  changedStrings <- highlight_stringdiff_iter(
    prior = script_prior[lineMatches][changes],
    after = script_after[lineMatches][changes],
    html = FALSE)

  cat(paste(paste("Line ", line_matches_pos[changes],
                  ": ", changedStrings, sep = ""),
            collapse = "\n"), "\n")

  # lines where a function name occurred, but no changed happened
  # not comprehensive, since there might be line where one function was
  # recognized, but not another
  potential_missings <- script_after[lineMatches]#[!changes]
  # color missed functions, like crayon::yellow("TEST)
  replacementRegex <- paste0("\\1\033[33m\\2\033[39m\\3")
  potential_missings_unchanged <-
    # iterate over each line
    purrr::map_chr(
      .x = potential_missings,
      .f = function(LINE) {
        # iterate over each function
        # using reduce to not override prevoius changed functions
        purrr::reduce(.x = functionsInScript,
                      .f = function(STRING, FUN) {

                        # gsub only changes the first occurrence
                        # hence a repeat loop
                        # potentially changes this code part

                        # function name which does not come after "::"
                        # or a color sequenz
                        FUN <- gsub("\\.", "\\\\.", x = FUN)
                        patternRegex <- paste0("(.*)((?<!::|33m)", FUN, ")(.*)")
                        repeat{
                          STRING_PRIOR <- STRING
                          STRING <- gsub(x = STRING,
                                         replacement = replacementRegex,
                                         pattern = patternRegex,
                                         perl = TRUE)
                          if (STRING_PRIOR == STRING) {
                            break
                          }
                        }

                        return(STRING)
                      },
                      .init = LINE)
      })

  potential_missings_unchanged <- potential_missings_unchanged[
    potential_missings_unchanged != potential_missings]
  if (length(potential_missings_unchanged) > 0) {
    cat("\n", crayon::blue("Function names are not used like functions.",
                           "Check for variable names or",
                          "functional programming in *apply/purrr"), "\n")
    cat(paste(paste("Line ", line_matches_pos, #[!changes],
                    ": ", potential_missings_unchanged, sep = ""),
              collapse = "\n"), "\n")
  }

  # did special functions such as "%like" or %>% which are not used with
  # PACAKGE::FUNCTION occur
  if (any(special_matches)) {
    cat("\n", crayon::magenta("Special functions used!"), "\n")

    special_functions_in_script <- functions[special_functions][special_matches]
    # lines with special functions
    specialMatches <- which(as.logical(
      Reduce(f = "+", purrr::map(.x = special_functions_in_script,
                                 .f = ~ grepl(x = script_after,
                                              pattern = .x,
                                              fixed = TRUE)))
      ))
    # which special functions were used?
    # Through the special characters a regular expression search is not possible
    # Hence `fixed = TRUE` and looping overall possible functions
    # each list element is a special function and the number represent the line
    specialsFound <- purrr::map(.x = special_functions_in_script,
                                .f = ~grep(x =  script_after[specialMatches],
                                           pattern = .x, fixed = TRUE))

    # function's name as name
    names(specialsFound) <- special_functions_in_script

    specialsUnlisted <- unlist(purrr::transpose(specialsFound))
    # aggregate all functions by lines
    funsInLine <- by(names(specialsUnlisted),
                     specialsUnlisted, paste, collapse = ", ")

    # for alignment, make all functions string the same length
    funsInLine <- format(funsInLine, width = max(nchar(funsInLine)))

    # Output highlighting not used because of special characters
    cat(paste(paste("Line ", specialMatches, ": ",
                    funsInLine, "\t",
                    script_after[specialMatches],
                    sep = ""),
              collapse = "\n"), "\n")
  }
  cat("\n\n")

}


highlight_stringdiff_iter <- function(prior, after, html = TRUE) {
  Map(f = function(p, a) highlight_stringdiff(prior = p,
                                              after = a,
                                              html = html),
      prior,
      after)
}

highlight_stringdiff <- function(prior, after, html = TRUE) {
  # include an empty string in prior
  # if there are differences between prior and after
  split_regex <- "(?=[[:space:]]|[[:punct:]])"
  prior_split <- strsplit(prior, split = split_regex, perl = TRUE)[[1]]
  after_split <- strsplit(after, split = split_regex, perl = TRUE)[[1]]
  for (i in seq_along(after_split)) {
    if (prior_split[i] != after_split[i]) {
      len_current <- length(prior_split)
      prior_split[i:(len_current + 1)] <- c("", prior_split[i:len_current])
    }
  }

  # colorize detected added tokens
  # see crayon::green("TEST)
  diffs <- !nzchar(prior_split)

  # to capture the first diff, adding a FALSE = 0
  pos <- diff(c(FALSE, diffs))

  if (html) {
    color_start <- '<text style="color:red;">'
    color_end <- "</text>"
  } else {
    color_start <- "\033[36m"
    color_end <- "\033[39m"
  }

  # +1 since the nth diff, means on the nth +1 position the change starts
  # not for the end, since the diff here means that the change has happened
  # on the nth position
  after_split[pos == 1] <- paste0(color_start, after_split[pos == 1])
  after_split[which(pos == -1) - 1] <-
    paste0(after_split[which(pos == -1) - 1], color_end)

  after <- paste(after_split, collapse = "")
  return(after)
}
