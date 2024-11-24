# set options on load
.onLoad <- function(libname, pkgname) {
  optns <- options()
  optns_origin <- list(
    origin.ask_before_applying_changes = TRUE,
    origin.use_markers_for_logging = TRUE,
    origin.pkgs = NULL,
    origin.color_added_package = "#00F9FF",
    origin.color_missed_function = "#ff0000",
    origin.color_infix_function = "#ffa500",
    origin.overwrite = TRUE,
    origin.check_conflicts = TRUE,
    origin.check_base_conflicts = TRUE,
    origin.add_base_packages = FALSE,
    origin.excluded_functions = list(),
    origin.check_local_conflicts = TRUE,
    origin.path_to_local_functions = NULL,
    origin.verbose = TRUE,
    origin.filetypes = "R"
  )

  to_set <- !(names(optns_origin) %in% names(optns))
  if (any(to_set)) {
    options(optns_origin[to_set])
  }

  return(invisible(NULL))
}
