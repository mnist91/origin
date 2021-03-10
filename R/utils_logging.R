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
prep_verbose <- function(script_prior,
                         lineMatches = rep(TRUE, length(script_after)),
                         functions,
                         functionsInScript,
                         special_functions = NULL,
                         special_matches = FALSE) {
  
  # lines where a function name occurred, but no changed happened
  # not comprehensive, since there might be line where one function was
  # recognized, but not another
  potential_missings <- script_prior[lineMatches]#[!changes]
  
  # check for functions 
  # special regex characters in functions like dots must be escaped
  # function names  should not be preceded by a double colon OR character nor
  # succeeded by a double colon OR a percentage sign OR a character
  funs_comb <- paste(functionsInScript, collapse = "|")
  funs_prep <- gsub("\\.", "\\\\.", x = funs_comb)
  fun_regex <- paste0("(?<!::|[[:alnum:]])(", funs_prep, ")(?!::|%|[[:alnum:]])")
  
  
  list_pot_missings <- get_matches(line = which(lineMatches),
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
    specialMatches <- which(as.logical(
      Reduce(f = "+", purrr::map(.x = special_functions_in_script,
                                 .f = ~ grepl(x = script_prior,
                                              pattern = .x,
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
    
    list_specials <- get_matches(line = specialMatches,
                                 text = script_prior[specialMatches],
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


# find potential missings in the data and store information
# about type of missing, length of match and line
get_matches <- function(text, line, regex,
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


highlight_stringdiff_iter <- function(prior, after, html = TRUE) {
  Map(f = function(p, a) highlight_stringdiff(prior = p,
                                              after = a,
                                              html = html),
      prior,
      after)
}

get_diffpos_iter <- function(prior, after, line) {
  diffpos <- unname(
    Map(f = function(p, a, l) get_diffpos(prior = p,
                                          after = a,
                                          line = l),
        prior,
        after,
        line)
  )
  return(diffpos)
}

get_diffpos <- function(prior, after, line) {
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
  
  
  # +1 since the nth diff, means on the nth +1 position the change starts
  # not for the end, since the diff here means that the change has happened
  # on the nth position
  start_pos <- which(pos == 1)
  end_pos <- which(pos == -1) - 1
  return(list(start_pos = start_pos,
              end_pos = end_pos,
              line = line))
}
