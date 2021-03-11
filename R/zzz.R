# set options on load
.onLoad <- function(libname, pkgname) {
  optns <- options()
  optns_origin <- list(
    origin.ask_before_applying_changes = TRUE
  )
  to_set <- !(names(optns_origin) %in% names(optns))
  if (any(to_set)) options(optns_origin[to_set])
  invisible(NULL)
}