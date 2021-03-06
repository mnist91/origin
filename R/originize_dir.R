#' Add Explicit Package Names to its Functions
#'
#'
#' @param path path to a directory
#' @template pkgs
#' @param recursive logical. Should the listing recurse into directories?
#'  See \link[base]{list.files}
#' @param files_pattern A regular expression. Only file names which match the
#'  regular expression will be returned. See \link[base]{list.files}
#' @param ignore_case logical. Should pattern-matching be case-insensitive?
#'  See \link[base]{list.files}
#' @param exclude_files list of files to be excluded
#' @template overwrite
#' @template ask_before_applying_changes
#' @template ignore_comments
#' @template check_conflicts
#' @template check_base_conflicts
#' @template add_base_packages
#' @template excluded_functions
#' @template verbose
#' @template use_markers
#' @template check_local_funs
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
           use_markers = getOption("origin.use_markers_for_logging", TRUE),
           check_local_funs = getOption("origin.check_local_funs", TRUE)
  ) {

    if (!check_base_conflicts && add_base_packages) {
      stop("When adding base packages checking for ",
           "potential conflicts is required!")
    }

    files <- list_files(path = path,
                        exclude_folders = c("renv", "packrat"),
                        full.names = TRUE,
                        include.dirs = FALSE,
                        recursive = recursive,
                        pattern = files_pattern,
                        ignore.case = ignore_case)


    # TODO: non absolute paths
    if (!is.null(exclude_files)) {
      if (any(!exclude_files %in% files)) {
        stop("File to exclude not in given path\n",
             exclude_files[!exclude_files %in% files])
      }
      files <- files[!files %in% exclude_files]
    }


    # warning if many files are about to be originized
    n_files <- length(files)
    if (n_files > 20) {
      cat(sprintf("You are about to originize %s files.\nProceed?", n_files))
      if (interactive()) {
        answer <- menu(choices = c("YES", "NO", "Show files")) # nocov
      } else {
        answer <- 1
      }

      if (answer == 2) {
        stop("Execution halted") # nocov
      } else if (answer == 3) {
        print(files)
        cat("\nProceed?")
        answer2 <- menu(choices = c("YES", "NO")) # nocov

        if (answer2 == 2) {
          stop("Execution halted") # nocov
        }
      }
    }

    # read file
    scripts <- lapply(files, readLines)

    # check for empty scripts
    empty_scripts <- vapply(X = scripts, FUN = length, FUN.VALUE = integer(1)) == 0

    if (all(empty_scripts)) {
      message("All provided scripts are empty")
      return(invisible(NULL))
    } else if (any(empty_scripts)) {
      scripts <- scripts[!empty_scripts]
      files <- files[!empty_scripts]
    }

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
                   use_markers = use_markers,
                   check_local_funs = check_local_funs)

    return(invisible(NULL))

  }

