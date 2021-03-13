#' Add Explicit Package Names to its Functions
#'
#' @param file a path to a script
#' @param pkgs a vector with package names
#' @param overwrite a boolean, if TRUE the file will be saved and overwritten. If FALSE the file is returned.
#' @param ignoreComments a boolean, if TRUE lines starting with # are ignored
#' @param excludeBasePackages a boolean, if TRUE base R functions are excluded
#' @param verbose a boolean
#'
#' @return
#' @export
#'
originize_selection <- function(context = rstudioapi::getSourceEditorContext(),
                                pkgs = .packages(),
                                overwrite = TRUE,
                                ask_before_applying_changes = getOption("origin.ask_before_applying_changes"),
                                ignoreComments = TRUE,
                                check_conflicts = TRUE,
                                check_base_conflicts = TRUE,
                                add_base_packages = FALSE,
                                excluded_functions = list(),
                                verbose = TRUE,
                                use_markers = getOption("origin.use_markers_for_logging")) {
  
  if (!check_base_conflicts && add_base_packages) {
    stop("When adding base packages checking for potential conflicts is required!")
  }
  
  file <- context$path
  
  # if funciton is run in a unsaved script, use root path a dummy path
  # needed for sourceMarkers
  if (file == "") {
    # TODO: .
    file <- "."
  }
  
  init_text <- context$selection[[1]]$text
  # read file
  script <- strsplit(init_text, split = "\\n")[[1]]
  
  # TODO: save as internal data
  base_r_packages <- c("stats", "graphics", "grDevices", "datasets",
                       "utils", "methods", "base")
  
  # exclude base R packages from checks for duplicates
  if (!check_base_conflicts) {
    pkgs <- setdiff(pkgs, base_r_packages)
  }
  
  # get all exported functions from each package
  functions <- setNames(object = lapply(X   = pkgs, 
                                        FUN = getFunctions), 
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
                                 FUN = function(FUN) {
                                   grepl(pattern = FUN,
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
                      ignoreComments = ignoreComments,
                      verbose = verbose,
                      use_markers = use_markers)
  
  if (verbose) {
    selected_context <- context$selection[[1]]$range
    selected_lines <- selected_context$start[1]:selected_context$end[1]
    # in case the last line has been empty, strsplit does not create an empty
    # character. Hence, the script object is one element shorter than 
    # lines swlected and the assignment would fial
    
    result$logging_data$line <- selected_lines[result$logging_data$line]
    if (use_markers) {
      
      rstudioapi::sourceMarkers(name = "origin", markers = result$logging_data)
    } else {
      # TODO
    }
  }
  

  
  out <- paste(result$to_write$script, collapse = "\n")
  
  rstudioapi::insertText(out)
  
  return(invisible(NULL))
}
