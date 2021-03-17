#' Originize all files in the current R Project
#'
#' @param path defualts to the project root path
#' @template  pkgs 
#' @param recursive see \link[base]{list.files}
#' @param files_pattern see \link[base]{list.files}
#' @param ignore_case see \link[base]{list.files}
#' @param exclude_files see \link[base]{list.files}
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
originize_current_project <- 
  function(path = ".",
           pkgs = getOption("origin.pkgs"),
           recursive = TRUE,
           files_pattern = "\\.R",
           ignore_case = TRUE,
           exclude_files = NULL,
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
    
    originize_dir(path = path,
                  pkgs = pkgs,
                  recursive = recursive,
                  files_pattern = files_pattern,
                  ignore_case = ignore_case,
                  exclude_files = exclude_files,
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
