get_packages <- function() {
  pkgs <- .packages()
  
  crnt_project_pkg <- get_project_pkg()
  
  if (is.null(crnt_project_pkg)) {
    return(pkgs)
  } else {
    return(setdiff(pkgs, crnt_project_pkg))
  }
}
