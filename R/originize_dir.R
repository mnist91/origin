#' Add Explicit Package Names to its Functions
#'
#'
#' @param path path to a directory
#' @template pkgs
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
#' @return NULL
#' @export
#'
originize_dir <-
  function(path = ".",
           pkgs = getOption("origin.pkgs", .packages()),
           recursive = TRUE,
           files_pattern = "\\.R$",
           ignore_case = TRUE,
           exclude_files = NULL,
           overwrite = getOption("origin.overwrite", TRUE),
           ask_before_applying_changes =
             getOption("origin.ask_before_applying_changes", TRUE),
           ignore_comments = getOption("origin.ignore_comments", TRUE),
           check_conflicts = getOption("origin.check_conflicts", TRUE),
           check_base_conflicts = getOption("origin.check_base_conflicts", TRUE),
           add_base_packages = getOption("origin.add_base_packages", FALSE),
           excluded_functions = getOption("origin.excluded_functions", list()),
           verbose = getOption("origin.verbose", FALSE),
           use_markers = getOption("origin.use_markers_for_logging", TRUE)) {

    if (!check_base_conflicts && add_base_packages) {
      stop("When adding base packages checking for ",
                  "potential conflicts is required!")
    }

    files <- list.files(path = path,
                        full.names = TRUE,
                        include.dirs = FALSE,
                        recursive = recursive,
                        pattern = files_pattern,
                        ignore.case = ignore_case)


    # TODO: non absolute paths
    if (any(!exclude_files %in% files)) {
      stop("File to exclude not in given path\n",
           exclude_files[!exclude_files %in% files])
    }

    # read file
    scripts <- lapply(files, readLines)

    originize_wrap(scripts = scripts,
                   files = files,
                   type = "writeLines",
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
