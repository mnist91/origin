#' Find and exclude local functions
#'
#' check if locally defined functions share names with exported functions
#' from checked packages.
#' Note that all projects R scripts are searched for function definitions
#'
#' @param functions list of functions
#' @param files vector of file names
#' @param scripts list of r scripts
#' @param path_to_local_functions specific path to loacal functions.
#'   Defaults to "."
#' @template ask_before_applying_changes
#'
#' @return cleaned functions-list
#' @noRd
exclude_local_functions <- function(functions,
                                    files,
                                    scripts,
                                    path_to_local_functions,
                                    script_collapsed,
                                    ask_before_applying_changes = TRUE) {
  # get root path of the current project
  if (is.null(path_to_local_functions)) {
    project_path <- try(rstudioapi::getActiveProject())

    # In case R is not run from wihtin RStudio or origin is called from
    # within a project, inform the user and determine the root path
    # by the shared root path of all files.
    project_path_found <- TRUE
    if (inherits(project_path, "try-error")) {
      project_path_found <- FALSE
      warning(paste("RStudio not running. Hence, no project path to",
                    "search for local functions can be determined."))

      # nocov start
    } else if (is.null(project_path)) {
      project_path_found <- FALSE
      warning(paste("origin not run from within a project.",
                    "Cannot check for local functions"))
    }
    # nocov end

    # Are all checked files in the current project?
    # It is possible to originize one project from within another project
    # Then, it is unclear which local functions are to consider and
    # the check is skipped
    if (project_path_found &&
        # nocov start
        !all(not_in_project <- startsWith(x = normalizePath(files,
                                                            winslash = "/"),
                                          prefix = project_path))
    ) {
      project_path_found <- FALSE
      warning(sprintf(paste("%s files are not in the current",
                            "project path %s.\n",
                            "Cannot check for local functions due to",
                            "unclear root directory."),
                      length(not_in_project),
                      project_path))
    }

  } else {
    # a directory is provided
    project_path_found <- TRUE
    project_path <- path_to_local_functions
  }
  # nocov end


  if (project_path_found &&
      # nocov start
      !is.null(project_path) &&
      !is.na(project_path) &&
      nzchar(project_path)) {

    # locally defined functions
    local_funs <-
      get_local_functions(path = project_path)

    if (length(local_funs) > 0) {
      # overlaps of local and exported functions
      dups <- get_fun_duplicates(c(list(local = local_funs),
                                   functions))
      local_dups <- dups[dups %in% dups[names(dups) == "local"]]

      # TODO: is this string-matching check necessary?
      #       alternative: real function check
      # in case there is an overlap, check whether these overlapping
      # functions are actually in use
      if (length(local_dups) > 0) {

        # are any masked functions used in the currently checked script(s)
        local_dups <- sort(local_dups[names(local_dups) != "local"])

        local_dups_with_pkg <-
          stats::setNames(object = unique(local_dups),
                          nm = by(data = names(local_dups),
                                  INDICES = local_dups,
                                  FUN = paste,
                                  collapse = ", "))

        # just for logging purpose: consider only those functions
        # which are potentially in use
        local_dup_funs_in_script <- vapply(X = local_dups_with_pkg,
                                           FUN = function(f) {
                                             grepl(pattern = f,
                                                   x = script_collapsed,
                                                   fixed = TRUE)
                                           },
                                           FUN.VALUE = logical(1),
                                           USE.NAMES = TRUE)

        if (any(local_dup_funs_in_script)) {
          # inform the user
          solve_local_duplicates(
            local_dups_with_pkg[local_dup_funs_in_script],
            ask_before_applying_changes = ask_before_applying_changes)

          # exclude these local functions from originizing
          functions <-
            exclude_functions(
              functions,
              list(unname(local_dups_with_pkg[local_dup_funs_in_script])))
        }
      }
    }
  }
  # nocov end

  return(functions)
}
