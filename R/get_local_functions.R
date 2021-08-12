#' Find all user defined functions in the project
#'
#' @param path Path in which all defined function names should be
#'  found and retrieved.
#'
#' @return character vector of function names
#' @export
#'
#' @examples
#' get_local_functions(path = ".")
#' get_local_functions(path = rprojroot::find_rstudio_root_file())
get_local_functions <- function(path = ".") {

  # get project root file
  root <- try(path)

  # in case ther eis no root file, return nothing
  if (inherits(root, what = "try-error")) {
    return(character(0))
  }

  # all R files that do not live in the foldrs renv/packrat/tests
  files <- list_files(path = root,
                      exclude_folders = c("renv", "packrat", "tests"),
                      full.names = TRUE,
                      include.dirs = FALSE,
                      recursive = TRUE,
                      pattern = "\\.R$",
                      ignore.case = TRUE)



  # read files
  scripts <- suppressWarnings(lapply(X = files, FUN = readLines))

  # find funciton names
  local_funs <- sort(unique(unlist(lapply(X = scripts, FUN = find_functions))))

  return(local_funs)
}
