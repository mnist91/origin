#' Wrapper function to be used as an RSTUDIO ADDIN
#'
#' @param context information of marked editor section in RStudio
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
#' @return NULL
originize_selection <-
  function(context = rstudioapi::getSourceEditorContext(),
           pkgs = getOption("origin.pkgs", .packages()),
           overwrite = getOption("origin.overwrite"),
           ask_before_applying_changes =
             getOption("origin.ask_before_applying_changes"),
           ignore_comments = getOption("origin.ignore_comments"),
           check_conflicts = getOption("origin.check_conflicts"),
           check_base_conflicts = getOption("origin.check_base_conflicts"),
           add_base_packages = getOption("origin.add_base_packages"),
           excluded_functions = getOption("origin.excluded_functions"),
           verbose = getOption("origin.verbose"),
           use_markers = getOption("origin.use_markers_for_logging")) {

    file <- context$path

    # if funciton is run in a unsaved script, use root path a dummy path
    # needed for sourceMarkers
    if (file == "") {
      # TODO: .
      file <- "."
    }

    init_text <- context$selection[[1]]$text
    # read file
    script <- strsplit(init_text, split = "\\n")[[1]]

    if (verbose) {
      selected_context <- context$selection[[1]]$range
      selected_lines <- selected_context$start[1]:selected_context$end[1]
    }


    originize_wrap(scripts = list(script),
                   files = file,
                   type = "insertText",
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
                   selected_lines = selected_lines,
                   context = context)

    return(invisible(NULL))
  }
