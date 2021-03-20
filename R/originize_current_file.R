#' Add Explicit Package Names to its Functions
#'
#' Simple wrapper to originize_file with default of currently opened script.
#' Mainly for usage as an RStudio Addin.
#'
#' @template file
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
#' @return
#' @noRd
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
           verbose = TRUE,
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
