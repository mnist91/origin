#' Find All User Defined functions in the Project
#'
#' @param path Path in which all defined function names should be
#'  found and retrieved. Defaults to the current working directory.
#' @param time_limit seconds to wait for the function to finish
#'
#' @return character vector of function names
#' @export
#'
#' @examples
#' get_local_functions(path = ".")
#' get_local_functions(path = rstudioapi::getActiveProject())
get_local_functions <- function(path = ".", time_limit = 30L) {

  # get project root file
  root <- try(path)

  # in case ther eis no root file, return nothing
  if (inherits(root, what = "try-error")) {
    return(character(0))
  }


  # all R files that do not live in the foldrs renv/packrat/tests
  files <- list_files(path = root,
                      exclude_folders = c("renv", "packrat", "tests", ".git", ".Rproj"),
                      exclude_symlinks = TRUE,
                      full.names = TRUE,
                      include.dirs = FALSE,
                      recursive = TRUE,
                      pattern = "\\.R$",
                      ignore.case = TRUE)


  # read files and find function names
  local_funs <- suppressWarnings(
    sort(unique(unlist(
      lapply(X = files,
             FUN = function(x) find_functions(readLines(x)))
    )))
  )

  return(local_funs)
}
