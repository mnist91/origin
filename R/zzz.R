# set options on load
.onLoad <- function(libname, pkgname) {
  optns <- options()
  optns_origin <- list(
    origin.ask_before_applying_changes = TRUE,
    origin.use_markers_for_logging = TRUE,
    origin.pkgs = .packages()),
    origin.color_added_package = "#00F9FF",
    origin.color_missed_function = "#ffa500",
    origin.color_special_function = "#b300b3"
  )
  to_set <- !(names(optns_origin) %in% names(optns))
  if (any(to_set)) options(optns_origin[to_set])
  invisible(NULL)
}
