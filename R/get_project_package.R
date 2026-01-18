get_project_pkg <- function() {

  if (!rstudioapi::isAvailable()) {
    return(NULL)
  }

  # only works from within an active rstudio session which covr does not provide
  # nocov start
  prjct <- rstudioapi::getActiveProject()

  # not in a r project
  if (is.null(prjct)) {
    return(NULL)
  }

  # list all files in the root path
  files <- list.files(prjct)

  # a DESCRIPTION file is needed for a package project
  if (!"DESCRIPTION" %in% files) {
    return(NULL)
  }

  # read in the descirption file

  description <- readLines(file.path(prjct, "DESCRIPTION"))

  # a package project must start with ´Type: Package´
  if (description[1] != "Type: Package") {
    return(NULL)
  }

  pkg_name <- grep("^Package: ", description, value = TRUE)

  # ambiguous package name
  if (length(pkg_name) != 1) {
    return(NULL)
  }

  # retreive package name
  pkg_name <- gsub("Package: ", "", pkg_name, fixed = TRUE)

  return(pkg_name)
  # nocov end

}
