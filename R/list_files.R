#' List all files in a directory by allow for excluding subdirectories
#'
#' @param path character string of path to list files from
#' @param exclude_folders character vector of folders to exlcude
#' @param ... arguments passed to \link[base]{list.files}
#'
#' @noRd
#' @return a character vector of file names
#'
#' @examples
#' \donttest{
#' # list all R files except from files in the project library
#' list_files(".",
#'            exclude_folders = c("renv", "packrat", "tests"),
#'            pattern = "\\.R$",
#'            ignore.case = TRUE)
#' }
list_files <- function(path,
                       exclude_folders = c("renv", "packrat", "tests"),
                       ...) {

  # in case not exlcudable cirectories are provided, just use list.files()
  if (length(exclude_folders) == 0) {
    return(list.files(path = path,
                      ...))
  }
  # list alle directories
  # it is faster to first exclude unwanted folders like renv right away in case
  # they are a direct subfolder of the root directory.
  dirs <- list.dirs(path = path,
                    full.names = TRUE,
                    recursive = FALSE)

  # exclude local project environment, usually  package handler and test folders
  dirs <-
    dirs[!grepl(x = dirs,
                pattern = paste(escape_strings(paste0(.Platform$file.sep,
                                                      exclude_folders)),
                                collapse = "|"))]

  # find all R scripts in the project
  files <- unlist(lapply(X   = dirs,
                         FUN = list.files,
                         ...))

  # in the path directory, search for R files as well
  # the recursive option must then be set to FALSE. Must have higher priority
  # than if set in the ... dots
  arguments <- c(list(path = path,
                      recursive = FALSE),
                 list(...))
  arguments <- arguments[!duplicated(names(arguments))]
  root_files <- do.call(what = "list.files",
                        args = arguments)

  # combine both file vectors
  files <- c(files, root_files)

  # in case excludable folders are not direct subdirectories of the root,
  # exclude them by checking the complete file names
  files <-
    files[!grepl(x = files,
                 pattern = paste(escape_strings(paste0(.Platform$file.sep,
                                                       exclude_folders,
                                                       .Platform$file.sep)),
                                 collapse = "|"))]

  return(files)
}













