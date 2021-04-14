#' Add Explicit Package Names to its Functions
#'
#'
#' @param scripts list of script(s) to originize
#' @template pkgs
#' @template overwrite
#' @template ask_before_applying_changes
#' @template ignore_comments
#' @template check_conflicts
#' @template check_base_conflicts
#' @template add_base_packages
#' @template excluded_functions
#' @template verbose
#' @template use_markers
#'
#' @return NULL
#' @export
#'
originize_wrap <-
  function(scripts,
           files,
           type,
           pkgs = getOption("origin.pkgs"),
           overwrite = TRUE,
           ask_before_applying_changes =
             getOption("origin.ask_before_applying_changes"),
           ignore_comments = TRUE,
           check_conflicts = TRUE,
           check_base_conflicts = TRUE,
           add_base_packages = FALSE,
           excluded_functions = list(),
           verbose = FALSE,
           use_markers = getOption("origin.use_markers_for_logging"),
           selected_lines = NULL) {

    if (!check_base_conflicts && add_base_packages) {
      stop("When adding base packages checking for ",
           "potential conflicts is required!")
    }

    if (ask_before_applying_changes && !verbose) {
      stop(paste("Without verbose == TRUE no changes are visible before",
                 "applying. `verbose` must be TRUE if",
                 "ask_before_applying_changes is TRUE"))
    }

    # exclude base R packages from checks for duplicates
    if (!check_base_conflicts) {
      pkgs <- setdiff(pkgs, base_r_packages)
    }

    if (length(pkgs) == 0) {
      stop(paste("No packages specified. Please use either",
                 "`options(origin.pkgs)` or the `pkgs` argument"))
    }

    # get all exported functions from each package
    functions <- setNames(object = lapply(X   = pkgs,
                                          FUN = get_exported_functions),
                          nm     = pkgs)

    if (length(functions) == 0) {
      stop("Given packages do no export functions")
    }

    # exclude unwanted functions
    if (!is.null(excluded_functions) && length(excluded_functions) > 0) {
      functions <- exclude_functions(functions, excluded_functions)
    }

    if (length(functions) == 0) {
      stop("You excluded all exported functions from the given packages")
    }


    # DUPLICATES ---------------------------------------------------------------
    # find functions, that are called within multiple packages
    # a automatic assignment is not possible in such cases
    # a deterministic order is chosen

    if (check_conflicts) {
      # get duplicate functions
      dups <- get_fun_duplicates(functions)

      script_collapsed <- paste(lapply(X = scripts,
                                       FUN = paste,
                                       collapse = ""),
                                collapse = "")
      # which duplicates are in the script
      dup_funs_in_script <- vapply(X = dups,
                                   FUN = function(f) {
                                     grepl(pattern = f,
                                           x = script_collapsed,
                                           fixed = TRUE)
                                   },
                                   FUN.VALUE = logical(1),
                                   USE.NAMES = TRUE)

      # Require User interaction if duplicates are detected
      if (any(dup_funs_in_script)) {
        solve_fun_duplicates(dups = dups[dup_funs_in_script],
                             pkgs = pkgs)
      }
    }

    # do not consider base packages in originizing
    if (!add_base_packages) {
      pkgs <- setdiff(pkgs, base_r_packages)
      functions <- functions[!names(functions) %in% base_r_packages]
    }

    if (length(pkgs) == 0) {
      stop(paste("No packages specified.Please use either `options(origin.pkgs)`",
                 "or the `pkgs` argument. If you desire to use base",
                 "packages, inspect the `add_base_packages` argument"))
    }


    if (length(functions) == 0) {
      stop("No non-excluded exported functions in given packages")
    }



    # apply originize function to each file/script
    results <- mapply(
      FUN = function(f, s) {
        originize(file = f,
                  script = s,
                  functions = functions,
                  pkgs = pkgs,
                  overwrite = overwrite,
                  ignore_comments = ignore_comments,
                  verbose = verbose,
                  use_markers = use_markers)
      },
      files,
      scripts,
      SIMPLIFY = FALSE,
      USE.NAMES = TRUE
    )

    # invoke logging
    if (verbose) {
      if (type == "insertText") {
        # in case the last line of a script is empty, strsplit does not create an empty
        # character. Hence, the script object is one element shorter than
        # lines selected and the assignment would fial
        results[[1]]$logging_data$line <- selected_lines[results[[1]]$logging_data$line]

        run_logging(results[[1]]$logging_data, use_markers = use_markers)
      } else {
        run_logging(Reduce(f = rbind,
                           x = lapply(X = results,
                                      FUN = function(l) l$logging_data)),
                    use_markers = use_markers)
      }
    }

    if (overwrite) {
      # overwrite script files
      if (type == "writeLines") {
        apply_changes(ask_before_applying_changes = ask_before_applying_changes,
                      result = results,
                      init_script = scripts)

        # return plane text
      } else if (type == "paste") {
        return(paste(results[[1]]$to_write$script, collapse = "\n"))

        # insert Text via apistudioapi
      } else if (type == "insertText") {
        to_insert <- paste(results[[1]]$to_write$script, collapse = "\n")
        rstudioapi::insertText(to_insert)

      }
    }

    return(invisible(NULL))
  }