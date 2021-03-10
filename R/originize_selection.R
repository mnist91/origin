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
originize_selection <- function(text = rstudioapi::getSourceEditorContext()$selection[[1]]$text,
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
  
  result <- originize_text(text = text,
                           file = file,
                           pkgs = pkgs,
                           overwrite = overwrite,
                           ask_before_applying_changes = ask_before_applying_changes,
                           ignoreComments = ignoreComments,
                           check_conflicts = check_conflicts,
                           check_base_conflicts = check_base_conflicts,
                           add_base_packages = add_base_packages,
                           excluded_functions = excluded_functions,
                           verbose = verbose,
                           html = html) 
  
  rstudioapi::insertText(result)
  
  invisible(return(NULL))
}
