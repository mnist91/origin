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
originize_text <- function(text,
                           file = rstudioapi::getSourceEditorContext()$path,
                           pkgs = .packages(),
                           overwrite = TRUE,
                           ask_before_applying_changes = TRUE,
                           ignoreComments = TRUE,
                           check_conflicts = TRUE,
                           check_base_conflicts = TRUE,
                           add_base_packages = FALSE,
                           excluded_functions = list(),
                           verbose = TRUE,
                           html = TRUE) {
  
  if (!check_base_conflicts && add_base_packages) {
    stop("When adding base packages checking for potential conflicts is required!")
  }
  
  # if funciton is run in a unsaved script, use root path a dummy path
  # needed for sourceMarkers
  if (file == "") {
    # TODO: .
    file <- "."
  }

  # read file
  script <- strsplit(text, split = "\\n")
  
  
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
                      html = html)
  
  if (verbose) {
    if (html) {
      rstudioapi::sourceMarkers(name = "origin", markers = result$logging_data)
    } else {
      # TODO
    }
  }
  

  
  
  return(paste(result$to_write$script, collapse = "\n"))
}
