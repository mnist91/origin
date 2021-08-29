#' Originize a specific file
#'
#' @template file
#' @template pkgs
#' @template overwrite
#' @template ask_before_applying_changes
#' @template ignore_comments
#' @template check_conflicts
#' @template check_base_conflicts
#' @template check_local_conflicts
#' @template path_to_local_functions
#' @template add_base_packages
#' @template excluded_functions
#' @template verbose
#' @template use_markers
#'
#' @details check_conflicts checks whether multiple packages listed in pkgs
#' export
#' functions with the same name, e.g. lag() is both part of the dplyr and
#' data.table namespace. If there are any conflicts actually present
#' in any considered script, these conflicts are shown including how origin
#' would solve them. User input is required to proceed. The order in pkgs
#' determines the precedence, while those listed first have higher precedence
#' than those listed later in the vector. This is consistent with function
#' masking in R.
#'
#' check_base_conflicts checks whether functions listed in pkgs mask R functions
#'  of R core packages (base, utils, stats, methods, graphics, grDevices,
#'  datasets). Even tough the user might not include those functions in the
#'  pkg::fct logic, potential conflicts require careful evaluation.
#'
#'
#' @return NULL
#' @export
#'
#' @examples
#' \dontrun{
#' originize_file(file = "originize_me.R",
#'                pkgs = c("dplyr", "data.table"),
#'                overwrite = TRUE,
#'                ask_before_applying_changes = TRUE,
#'                ignore_comments = TRUE,
#'                excluded_functions = list(dplyr = c("%>%", "tibble"),
#'                                          data.table = c(":=", "%like%"),
#'                                          # generally exclude
#'                                          c("last", "first")),
#'                verbose = TRUE)
#' }
originize_file <-
  function(
    file,
    pkgs = getOption("origin.pkgs", .packages()),
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
    path_to_local_functions = getOption("origin.path_to_local_functions", NULL),
    check_local_conflicts = getOption("origin.check_local_conflicts", TRUE)
  ) {

    if (!file.exists(file)) {
      stop("No file in this path\n", file)
    }


    # read file
    script <- suppressWarnings(readLines(file))


    originize_wrap(scripts = list(script),
                   files = file,
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
                   path_to_local_functions = path_to_local_functions,
                   check_local_conflicts = check_local_conflicts)

    return(invisible(NULL))

  }
