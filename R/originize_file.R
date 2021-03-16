#' Add Explicit Package Names to its Functions
#'
#' @param file a path to a script
#' @template pkgs
#' @param overwrite a boolean, if TRUE the file will be saved and overwritten.
#'   If FALSE the file is returned.
#' @param ignore_comments a boolean, if TRUE lines starting with # are ignored
#' @param excludeBasePackages a boolean, if TRUE base R functions are excluded
#' @template verbose
#'
#' @return
#' @export
#'
originize_file <- 
  function(file,
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
           use_markers = getOption("origin.use_markers_for_logging")) {
    
    if (!check_base_conflicts && add_base_packages) {
      stop("When adding base packages checking for ",
      "potential conflicts is required!")
    }
    
    if (!file.exists(file)) {
      stop("No file in this path\n", file)
    }
    
    # read file
    script <- readLines(file)
    
    
    # TODO: save as internal data
    base_r_packages <- c("stats", "graphics", "grDevices", "datasets",
                         "utils", "methods", "base")
    
    # exclude base R packages from checks for duplicates
    if (!check_base_conflicts) {
      pkgs <- setdiff(pkgs, base_r_packages)
    }
    
    # get all exported functions from each package
    functions <- setNames(object = lapply(X   = pkgs, 
                                          FUN = get_exported_functions), 
                          nm     = pkgs)
    
    # exclude unwanted functions
    if (!is.null(excluded_functions) && length(excluded_functions) > 0) {
      functions <- exclude_functions(functions, excluded_functions)
    }
    
    
    # DUPLICATES ---------------------------------------------------------------
    # find functions, that are called within multiple packages
    # a automatic assignment is not possible in such cases
    # a deterministic order is chosen
    
    if (check_conflicts) {
      # get duplicate functions
      dups <- get_fun_duplicates(functions)  
      
      script_collapsed <- paste(script, collapse = "")
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
    
    
    result <- originize(script = script,
                        file = file,
                        functions = functions,
                        pkgs = pkgs,
                        overwrite = overwrite,
                        ignore_comments = ignore_comments,
                        verbose = verbose,
                        use_markers = use_markers)
    
    if (verbose) {
      if (use_markers) {
        rstudioapi::sourceMarkers(name = "origin",
                                  markers = result$logging_data)
      } else {
        # TODO
        cat(paste(paste0(result$logging_data$line, ": ",
                         result$logging_data$message), 
                  collapse = "\n"))
      }
    }
    
    if (overwrite) {
      apply_changes(ask_before_applying_changes = ask_before_applying_changes,
                    result = result)
    }
    
    
    
    return(invisible(NULL))
  }
