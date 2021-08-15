#' Searches R Scripts for (Potential) Missings
#'
#' @param script script to originize
#' @param file file name of the script
#' @param functions list of function names to check
#' @param pkgs packages to consider
#' @param overwrite whether to overwrite files on disk
#' @param ignore_comments whether to ignore comments from originizing
#' @template verbose
#' @template use_markers
#'
#' @return
#' @noRd
originize <- function(script,
                      file,
                      functions,
                      pkgs = getOption("origin.pkgs", .packages()),
                      overwrite = FALSE,
                      ignore_comments = TRUE,
                      verbose = FALSE,
                      use_markers =
                        getOption("origin.use_markers_for_logging")) {


  # get relevant function information
  # TODO: still relevant / much time improvement here?
  fun_list <- check_functions(script = script,
                              functions = unlist(functions),
                              verbose = verbose,
                              ignore_comments = ignore_comments)

  if (length(fun_list$line_matches) == 0) {
    return(NULL)
  }
  # iterate over all functions and find position where package:: is necessary
  replacement_list <-
    Map(f = function(pkg, funs) {
      get_origins(pkg             = pkg,
                  script          = script,
                  functions       = funs,
                  file            = file,
                  overwrite       = overwrite,
                  ignore_comments = ignore_comments,
                  verbose         = verbose)
    },
    pkgs,
    functions
    )

  # keep package info only if matches are present
  replacement_list <- Filter(function(l) length(l) > 0, replacement_list)

  # combine results for all packages
  combined <- Reduce(f = function(...) {
    Map(f = c,
        ...)
  },
  x = replacement_list)

  # insert package:: to the relevant positions
  fixed_lines_list <- lapply(
    X = sort(unique(combined$line)),
    FUN = prep_line_originize,
    lines = combined$line,
    matches = combined$matches,
    pkg = combined$pkg,
    string = combined$string)

  # combine all lines
  fixed_lines_dat <- Reduce(rbind, fixed_lines_list)


  # if no logging is desired, skpi all relevant steps
  if (!verbose) {
    script[fixed_lines_dat$line] <- fixed_lines_dat$string

    return(list(to_write = list(file = file, script = script),
                logging_data = data.frame(line = fixed_lines_dat$line)))

  } else {

    # in case there are lines with html-characters which might intervene with
    # the markers logging output eventually, escape them
    script_logging <- script
    if (use_markers &&
        (has_html <-
         length(is_html_line <- which(grepl(pattern = "<|>", x = script))) > 0)
    ) {
      script_logging[is_html_line] <-
        gsub(pattern     = ">",
             replacement = "&gt;",
             x           = gsub(pattern     = "<",
                                replacement = "&lt;",
                                x           = script[is_html_line]))
    }

    # get positions of potential missined (infix) functions
    potential_missings <-
      get_potential_missings(script = script_logging,
                             line_matches = fun_list$line_matches,
                             functions = unlist(functions),
                             functions_in_script = fun_list$functions_in_script,
                             infix_functions = fun_list$infix_functions,
                             infix_matches = fun_list$infix_matches)


    # the insertion positions must be adjustet to the escaped HTML-characters
    if (use_markers && has_html) {
      is_html <- gregexpr(pattern = "<|>",
                          text = script)
      replacement_list <-
        lapply(X = replacement_list,
               FUN = function(rl) {

                 # which lines in the script that have insertions
                 # for this package are with HTMLs
                 relevant_script <- intersect(x = rl$line,
                                              y = is_html_line)

                 # which lines with functions from this package
                 # have HTML-characters
                 relevant_replacement_line <- rl$line %in% is_html_line

                 if (length(relevant_script) > 0) {

                   # if any, iterate over these insertions
                   # and add 3 tokens to the matching position
                   # < -> &lt;
                   # > -> &gt;
                   # each plus three tokens
                   rl$matches[relevant_replacement_line] <-
                     Map(f = function(htmls, matches) {
                       unlist(lapply(X = matches,
                                     FUN = function(mtchs) {
                                       mtchs + sum(mtchs > htmls) * 3
                                     }))
                     },
                     is_html[relevant_script],
                     rl$matches[relevant_replacement_line])
                 }

                 return(rl)
               })
    }

    # combine positions of potential missings
    logging_comb <-  Reduce(
      f = function(...) {
        Map(f = c,
            ...)
      },
      x = c(replacement_list,
            Filter(f = Negate(is.null),
                   x = potential_missings)))

    # prepare strings and insert color highlighting where needed
    fixed_lines_list <- lapply(
      X = sort(unique(logging_comb$line)),
      FUN = prep_line_logging,
      lines = logging_comb$line,
      matches = logging_comb$matches,
      pkg = logging_comb$pkg,
      log_length = logging_comb$log_length,
      type = logging_comb$type,
      string = script_logging[logging_comb$line],
      use_markers = use_markers)

    # combine all lines
    logging_data <- Reduce(rbind, fixed_lines_list)

    if (use_markers && !is.null(logging_data)) {
      attr(logging_data$message, which = "class") <- c("html", "character")
    }

    logging_data$file <- file

    script[fixed_lines_dat$line] <- fixed_lines_dat$string

    return(list(to_write = list(file = file, script = script),
                logging_data = logging_data))
  }

}
