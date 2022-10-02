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
#' @template check_conflicts
#' @template check_base_conflicts
#' @template check_local_conflicts
#' @template path_to_local_functions
#' @template add_base_packages
#' @template excluded_functions
#' @template verbose
#' @template use_markers
#' @param selected_lines logical, only necessary for originize selection
#' @param context a document context regarding the selected r script
#'
#' @return either `NULL` if writing to a file or the originized file if
#'  overwrite selected/highlighted text.
#'
#' @noRd

# Huge complexity due to many checks
# Begin Exclude Linting
originize_wrap <-
  function(scripts,
           files,
           type,
           pkgs,
           overwrite = TRUE,
           ask_before_applying_changes =TRUE,
           check_conflicts = TRUE,
           check_base_conflicts = TRUE,
           add_base_packages = FALSE,
           excluded_functions = list(),
           verbose = FALSE,
           use_markers = TRUE,
           check_local_conflicts = TRUE,
           path_to_local_functions = NULL,
           selected_lines = NULL,
           context = NULL) {
    
    # Parameter checks ---------------------------------------------------------
    if (!is.null(path_to_local_functions) &&
        !dir.exists(path_to_local_functions)) {
      stop(paste("Given path_to_local_functions",
                 path_to_local_functions,
                 "does not exist.",
                 "Cannot check for local functions."))
    }
    
    if (!check_base_conflicts && add_base_packages) {
      stop("When adding base packages checking for ",
           "potential conflicts is required.")
    }
    
    if (ask_before_applying_changes && !verbose) {
      stop(paste("Without verbose == TRUE no changes are visible before",
                 "applying. `verbose` must be TRUE if",
                 "ask_before_applying_changes is TRUE."))
    }
    
    # make sure no package is considered multiple times
    if (any((dup_pkgs <- duplicated(pkgs)))) {
      warning("The following packages are provided more than once: ",
              paste(unique(pkgs[dup_pkgs]), collapse = ", "))
      pkgs <- unique(pkgs)
    }
    
    # exclude base R packages from checks for duplicates
    if (!check_base_conflicts) {
      pkgs <- setdiff(pkgs, c(getOption("defaultPackages"), "base"))
    }
    
    if (length(pkgs) == 0) {
      stop(paste("No packages specified. Please use either",
                 "`options(origin.pkgs = c('pkg', ...))`",
                 "or the `pkgs` argument."))
    }
    
    # get all exported functions from each package
    functions <- stats::setNames(object = lapply(X   = pkgs,
                                                 FUN = get_exported_functions),
                                 nm     = pkgs)
    
    if (length(unlist(functions)) == 0) {
      stop("Given packages do not export functions.")
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
    if (check_local_conflicts) {
      script_collapsed <- paste(lapply(X = scripts,
                                       FUN = paste,
                                       collapse = ""),
                                collapse = "")
      functions <- exclude_local_functions(functions,
                                           files,
                                           scripts,
                                           path_to_local_functions,
                                           script_collapsed)
    }
    
    
    # DUPLICATES ---------------------------------------------------------------
    # find functions, that are called within multiple packages
    # a automatic assignment is not possible in such cases
    # a deterministic order is chosen
    
    if (check_conflicts) {
      
      # get duplicate functions
      dups <- sort(get_fun_duplicates(functions))
      dups_with_pkg <-
        stats::setNames(object = unique(dups),
                        nm = by(names(dups), dups, paste, collapse = ", "))
      
      # parse all scripts
      script_parsed <- Reduce(f = rbind,
                              lapply(X = files,
                                     FUN = get_parsed_data))
      
      # which duplicates are in the script, independet of whther be used as a
      # regular function (SYMBOL_FUNCTION_CALL)
      dup_funs_in_script <- dups_with_pkg %in% script_parsed$text
      
      # Require User interaction if duplicates are detected
      if (any(dup_funs_in_script)) {
        solve_fun_duplicates(dups = dups_with_pkg[dup_funs_in_script],
                             pkgs = pkgs)
      }
    }
    
    # do not consider base packages in originizing
    if (!add_base_packages) {
      pkgs <- setdiff(pkgs, c(getOption("defaultPackages"), "base"))
      functions <- functions[!names(functions) %in%
                               c(getOption("defaultPackages"), "base")]
    }
    
    if (length(pkgs) == 0) {
      stop(
        paste(
          "No packages specified. Please use either",
          "`options(origin.pkgs = c('pkg', ...))`",
          "or the `pkgs` argument. If you desire to use base",
          # lintr considers this as a file path
          "packages, inspect the `add_base_packages` argument/option.")) # Exclude Linting
    }
    
    
    if (length(unlist(functions)) == 0) {
      stop("No non-excluded exported functions in given packages.")
    }
    
    # reduce checked functions to matched names in any part of any script
    
    # in case this full script collapsing has not happend earlier.
    # better performance if an error is triggered prior to this step
    if (!exists("script_parsed")) {
      # parse all scripts
      script_parsed <- Reduce(f = rbind,
                              lapply(X = files,
                                     FUN = get_parsed_data))
    }
    
    # TODO: still time improvement?
    functions <- lapply(functions,
                        FUN = function(funs) {
                          funs[funs %in% script_parsed$text]
                        })
    
    # keep relevant packages only,
    # i.e. packages that ecport functions that might be used
    functions <- Filter(function(x) length(x) > 0,
                        functions)
    
    # nothing to log
    if (length(functions) == 0) {
      message("Nothing detected")
      return(NULL)
    }
    
    results <- originize(dat = script_parsed,
                         functions = functions,
                         pkgs = names(functions),
                         verbose = verbose,
                         use_markers = use_markers)
    # apply originize function to each file/script
    # results <- mapply(
    #   FUN = function(f, s) {
    #     originize(file = f,
    #               script = s,
    #               functions = functions,
    #               pkgs = names(functions),
    #               verbose = verbose,
    #               use_markers = use_markers)
    #   },
    #   files,
    #   scripts,
    #   SIMPLIFY = FALSE,
    #   USE.NAMES = TRUE
    # )
    
    # nothing to log
    if (length(results$logging_data$file) == 0) {
      message("Nothing detected")
      return(NULL)
    }
    
    # Empty Lines --------------------------------------------------------------
    # empty lines at the ending of the scirpt are excluded by parseData.
    lines_to_append <- lapply(scripts, function(x) {
      empty_lines <- !nzchar(x)
      pos_empty_lines <- which(empty_lines)
      pos_filled_lines <- which(!empty_lines)
      to_append <- pos_empty_lines[pos_empty_lines > max(pos_filled_lines)]
      return(to_append)
    })
    
    lines_to_append <- setNames(lines_to_append, files)
    
    
    lines_to_append <- Filter(function(x) length(x)  > 0, lines_to_append)

    if (length(lines_to_append) > 0) {
      for (file in files) {
        results$to_write[[file]][lines_to_append[[file]]] <- ""
      }
    }
    
    # invoke logging
    if (verbose) {
      if (type == "insertText") {
        # in case the last line of a script is empty, strsplit does not create
        # an empty character. Hence, the script object is one element shorter
        # than lines selected and the assignment would fial
        results$logging_data$line <-
          selected_lines[results$logging_data$line]
        
        run_logging(results$logging_data, use_markers = use_markers)
      } else {
        run_logging(
          results$logging_data,
          use_markers = use_markers)
      }
    }
    
    if (overwrite) {
      # overwrite script files
      out <- apply_changes(
        ask_before_applying_changes = ask_before_applying_changes,
        result = results,
        init_script = scripts,
        type = type,
        context = context)
      
    } else {
      out <- NULL
    }
    
    return(invisible(out))
  }
# End Exclude Linting
