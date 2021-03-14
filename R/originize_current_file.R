#' Add Explicit Package Names to its Functions
#'
#' Simple wrapper to originize_file with default of currently opened script. 
#' Mainly for usage as an RStudio Addin.
#'
#' @param file a path to a script
#' @param pkgs a vector with package names
#' @param overwrite a boolean, if TRUE the file will be saved and overwritten.
#'    If FALSE the file is returned.
#' @param ask_before_applying_changes a boolean. If TRUE, the user has to approve
#'   changes made by origin prior to applying them
#' @param ignore_comments a boolean, if TRUE, lines starting with # are ignored
#' @param check_conflicts a boolean. If TRUE, possible namespace conflicts are
#'   checked and, if there are any, user input is required. See also
#' @param check_base_conflicts a boolean. If TRUE; base R functions are also
#'   included in checking for conflicts
#' @param add_base_packages a boolean. If TRUE, base R functions are handled
#'   like all other packages and added via `::`
#' @param excluded_functions a list. Either an unnamed list of function names
#'   as strings. Then these functions are excluded from all packages and never 
#'   considered in origin. Or a named list with character vectors, Then 
#'   the name of the list element refers to a package and the given functions 
#'   are only excluded from this package. A very explicit way to handle
#'   namespace conflicts
#' @param verbose a boolean. If TRUE, origin provides feedback about its steps.
#' @param use_markers a boolean. If TRUE, the markers tab inn RStudio is used
#'   to track changes and show issues. FALSE prints the same informaiton in
#'   the console.
#'
#' @return
#' @export
#'
originize_current_file <- 
  function(file = rstudioapi::getSourceEditorContext()$path,
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
    
    originize_file(file,
                   pkgs = pkgs,
                   overwrite = overwrite,
                   ask_before_applying_changes = ask_before_applying_changes,
                   ignore_comments = ignore_comments,
                   check_conflicts = check_conflicts,
                   check_base_conflicts = check_base_conflicts,
                   add_base_packages = add_base_packages,
                   excluded_functions = excluded_functions,
                   verbose = verbose,
                   use_markers = use_markers) 
  }
