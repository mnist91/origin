#' Add Explicit Package Names to its Functions
#'
#'
#' @param scripts list of script(s) to originize
#' @param files character vector of file paths to originize
#' @param type character type of final output, either paste, insertText, or
#'   writeLines
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
#' @template check_local_funs
#' @param selected_lines logical, only necessary for originize selection
#' @param context a document context regarding the selected r script
#'
#' @return NULL
#' @noRd
originize_wrap <-
  function(scripts,
           files,
           type,
           pkgs = getOption("origin.pkgs", .packages()),
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
           check_local_funs = TRUE,
           selected_lines = NULL,
           context = NULL) {

    if (!check_base_conflicts && add_base_packages) {
      stop("When adding base packages checking for ",
           "potential conflicts is required.")
    }

    if (ask_before_applying_changes && !verbose) {
      stop(paste("Without verbose == TRUE no changes are visible before",
                 "applying. `verbose` must be TRUE if",
                 "ask_before_applying_changes is TRUE."))
    }

    # exclude base R packages from checks for duplicates
    if (!check_base_conflicts) {
      pkgs <- setdiff(pkgs, c(getOption("defaultPackages"), "base"))
    }

    if (length(pkgs) == 0) {
      stop(paste("No packages specified. Please use either",
                 "`options(origin.pkgs = c('pkg', ...))` or the `pkgs` argument."))
    }

    # get all exported functions from each package
    functions <- setNames(object = lapply(X   = pkgs,
                                          FUN = get_exported_functions),
                          nm     = pkgs)

    if (length(unlist(functions)) == 0) {
      stop("Given packages do no export functions.")
    }

    # exclude unwanted functions
    if (length(excluded_functions) > 0) {
      functions <- exclude_functions(functions, excluded_functions)
    }

    if (length(unlist(functions)) == 0) {
      stop("You excluded all exported functions from the given packages.")
    }

    # check if locally defined functions share names with exported functions
    # from checked packages.
    # Note that all projects R scripts are searched for function definitions
    if (check_local_funs) {

      # locally defined functions
      local_funs <- get_local_functions(path = rprojroot::find_rstudio_root_file())

      if (length(local_funs) > 0) {
        # overlaps of local and exported functions
        dups <- get_fun_duplicates(c(list(local = local_funs),
                                     functions))
        local_dups <- dups[dups %in% dups[names(dups) == "local"]]

        # in case there is an overlap
        if (length(local_dups) > 0) {

          script_collapsed <- paste(lapply(X = scripts,
                                           FUN = paste,
                                           collapse = ""),
                                    collapse = "")

          # are any masked functions used in the durrently checks script(s)
          local_dups <- sort(local_dups[names(local_dups) != "local"])
          local_dups_with_pkg <- setNames(object = unique(local_dups),
                                          nm = by(data = names(local_dups),
                                                  INDICES = local_dups,
                                                  FUN = paste,
                                                  collapse = ", "))

          local_dup_funs_in_script <- vapply(X = local_dups_with_pkg,
                                             FUN = function(f) {
                                               grepl(pattern = f,
                                                     x = script_collapsed,
                                                     fixed = TRUE)
                                             },
                                             FUN.VALUE = logical(1),
                                             USE.NAMES = TRUE)

          if (any(local_dup_funs_in_script)) {
            # inform the user
            solve_local_duplicates(dups = local_dups_with_pkg[local_dup_funs_in_script])

            # exclude these local functions from originizing
            functions <- exclude_functions(functions, list(unname(local_dups_with_pkg[local_dup_funs_in_script])))
          }
        }
      }
    }
    # DUPLICATES ---------------------------------------------------------------
    # find functions, that are called within multiple packages
    # a automatic assignment is not possible in such cases
    # a deterministic order is chosen

    if (check_conflicts) {
      # get duplicate functions
      dups <- sort(get_fun_duplicates(functions))
      dups_with_pkg <- setNames(object = unique(dups),
                                nm = by(names(dups), dups, paste, collapse = ", "))

      if (!exists("script_collapsed")) {
        script_collapsed <- paste(lapply(X = scripts,
                                         FUN = paste,
                                         collapse = ""),
                                  collapse = "")
      }

      # which duplicates are in the script
      dup_funs_in_script <- vapply(X = dups_with_pkg,
                                   FUN = function(f) {
                                     grepl(pattern = f,
                                           x = script_collapsed,
                                           fixed = TRUE)
                                   },
                                   FUN.VALUE = logical(1),
                                   USE.NAMES = TRUE)

      # Require User interaction if duplicates are detected
      if (any(dup_funs_in_script)) {
        solve_fun_duplicates(dups = dups_with_pkg[dup_funs_in_script],
                             pkgs = pkgs)
      }
    }

    # do not consider base packages in originizing
    if (!add_base_packages) {
      pkgs <- setdiff(pkgs, c(getOption("defaultPackages"), "base"))
      functions <- functions[!names(functions) %in% c(getOption("defaultPackages"), "base")]
    }

    if (length(pkgs) == 0) {
      stop(paste("No packages specified. Please use either `options(origin.pkgs = c('pkg', ...))`",
                 "or the `pkgs` argument. If you desire to use base",
                 "packages, inspect the `add_base_packages` argument/option.")) # Exclude Linting
    }


    if (length(unlist(functions)) == 0) {
      stop("No non-excluded exported functions in given packages.")
    }

    # reduce checked functions to matched names in any part of any script

    # in case this full script collapsing has not happend earlier.
    # better performance if an error is triggered prior to this step
    if (!exists("script_collapsed")) {
      script_collapsed <- paste(lapply(X = scripts,
                                       FUN = paste,
                                       collapse = ""),
                                collapse = "")
    }
    functions <- lapply(functions,
                        FUN = function(funs) {
                          funs[vapply(X = funs,
                                      FUN = function(f) {
                                        grepl(pattern = f,
                                              x = script_collapsed,
                                              fixed = TRUE)
                                      },
                                      FUN.VALUE = logical(1),
                                      USE.NAMES = TRUE)]
                        })

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

    # nothing to log
    if (all(vapply(X = results,
                   FUN = function(dat) is.null(dat$logging_data$line),
                   FUN.VALUE = logical(1)))) {
      message("Nothing detected")
      return(NULL)
    }

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
                           # exclude empty logs by Filter()
                           x = Filter(function(dat) !is.null(dat$line),
                                      lapply(X = results,
                                             FUN = function(l) l$logging_data))),
                    use_markers = use_markers)
      }
    }

    if (overwrite) {
      # overwrite script files
      out <- apply_changes(ask_before_applying_changes = ask_before_applying_changes,
                           result = results,
                           init_script = scripts,
                           type = type,
                           context = context)

    } else {
      out <- NULL
    }

    return(invisible(out))
  }
