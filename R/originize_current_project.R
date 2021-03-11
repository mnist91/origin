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
originize_current_project <- function(path = ".",
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
  
  originize_dir(path = path,
                pkgs = pkgs,
                recursive = recursive,
                files_pattern = files_pattern,
                ignore_case = ignore_case,
                exclude_files = exclude_files,
                overwrite = overwrite,
                ask_before_applying_changes = ask_before_applying_changes,
                ignoreComments = ignoreComments,
                check_conflicts = check_conflicts,
                check_base_conflicts = check_base_conflicts,
                add_base_packages = add_base_packages,
                excluded_functions = excluded_functions,
                verbose = verbose,
                html = html)
}