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
originize_dir <- function(path,
                          pkgs = .packages(),
                          recursive = TRUE,
                          files_pattern = "\\.R",
                          ignore_case = TRUE,
                          exclude_files = NULL,
                          overwrite = TRUE,
                          ask_before_applying_changes = getOption("origin.ask_before_applying_changes"),
                          ignoreComments = TRUE,
                          check_conflicts = TRUE,
                          check_base_conflicts = TRUE,
                          add_base_packages = FALSE,
                          excluded_functions = list(),
                          verbose = FALSE,
                          html = TRUE) {
  
  if (!check_base_conflicts && add_base_packages) {
    stop("When adding base packages checking for potential conflicts is required!")
  }
  
  files <- list.files(path = path,
                      full.names = TRUE,
                      include.dirs = FALSE,
                      recursive = recursive,
                      pattern = files_pattern, 
                      ignore.case = ignore_case)
  
  
  # TODO: non absolute paths
  if (any(!exclude_files %in% files)) {
    stop("File to exclude not in given path\n", exclude_files[!exclude_files %in% files])
  }
  
  # read file
  scripts <- lapply(files, readLines)

  
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
    
    script_collapsed <- paste(lapply(X = scripts,
                                     FUN = paste,
                                     collapse = ""),
                              collapse = "")
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
  

  # apply originize function to each file/script
  results <- mapply(
    FUN = function(f, s) {
      originize(file = f,
                script = s,
                functions = functions,
                pkgs = pkgs,
                overwrite = overwrite,
                ignoreComments = ignoreComments,
                verbose = verbose,
                html = html)
    },
    files,
    scripts,
    SIMPLIFY = FALSE,
    USE.NAMES = TRUE
  )

  
  
  if (verbose) {
    if (html) {
      rstudioapi::sourceMarkers(name = "origin",
                                markers = Reduce(f = rbind,
                                                 x = lapply(X = results, 
                                                            FUN = function(l) l$logging_data)))
    } else {
      # TODO
    }
  }
  
  if (overwrite) {
    apply_changes(ask_before_applying_changes = ask_before_applying_changes,
                  result = results)
  }
  
  
  
  return(invisible(NULL))
}