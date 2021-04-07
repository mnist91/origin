# Helper function
originize <- function(script,
                      file,
                      functions,
                      pkgs = getOption("origin.pkgs"),
                      overwrite = FALSE,
                      ignore_comments = TRUE,
                      verbose = FALSE,
                      use_markers =
                        getOption("origin.use_markers_for_logging")) {


  # get relevant function information
  l <- check_functions(script = script,
                       functions = unlist(functions),
                       verbose = verbose,
                       ignore_comments = ignore_comments)

  # iterate over all functions and find position where package:: is necessary
  replacement_list <-
    Map(f = function(pkg, funs) {
      get_origins(pkg            = pkg,
                  script         = script,
                  functions      = funs,
                  file           = file,
                  overwrite      = overwrite,
                  ignore_comments = ignore_comments,
                  verbose        = verbose)
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
                logging_data = data.frame()))

  } else {

    # get positions of potential missined (special) functions
    potential_missings <-
      get_potential_missings(script = script,
                             line_matches = l$line_matches,
                             functions = unlist(functions),
                             functions_in_script = l$functions_in_script,
                             special_functions = l$special_functions,
                             special_matches = l$special_matches)

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
      string = logging_comb$string,
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
