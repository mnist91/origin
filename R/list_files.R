#' List all files in a directory by allow for excluding subdirectories
#'
#' @param path character string of path to list files from
#' @param exclude_folders character vector of folders to exlcude,
#'   always case sensitive
#' @param exclude_symlinks logical whether to exclude symlinks from
#'   searching for files. Highly recommended to set to TRUE
#' @param pattern see \link[base]{list.files}
#' @param all.files see \link[base]{list.files}
#' @param full.names see \link[base]{list.files}
#' @param recursive see \link[base]{list.files}
#' @param ignore.case see \link[base]{list.files}
#' @param include.dirs see \link[base]{list.files}
#' @param no.. see \link[base]{list.files}
#'
#' @noRd
#' @return a character vector of file names
#'
#' @examples
#' # list all R files except from files in the subfolder library
#' list_files(R.home(),
#'            exclude_folders = c("library"),
#'            exclude_symlinks = TRUE,
#'            recursive = TRUE,
#'            full.names = TRUE,
#'            pattern = "\\.rds$",
#'            ignore.case = TRUE)
list_files <- function(path,
                       exclude_folders = c("renv",
                                           "packrat",
                                           "tests",
                                           ".git",
                                           ".Rproj"),
                       exclude_symlinks = TRUE,
                       pattern = NULL,
                       # Begin Exclude Linting
                       all.files = FALSE,
                       full.names = FALSE,
                       recursive = FALSE,
                       ignore.case = FALSE,
                       include.dirs = FALSE,
                       no.. = FALSE) {
  # End Exclude Linting
  # in case no excludable cirectories are provided and symlinks must not be
  # excluded, just use list.files()
  if ((no_excludes <- length(exclude_folders) == 0) && !exclude_symlinks) {
    files <- list.files(path = path,
                        pattern = pattern,
                        all.files = all.files,
                        full.names = full.names,
                        recursive = recursive,
                        ignore.case = ignore.case,
                        include.dirs = include.dirs,
                        no.. = no..)
    return(files)
  }

  # if no folders should be exldued and no recursive listing is desired
  # yet exclude symlinks
  if (!recursive && !no_excludes) {
    files <- list.files(path = path,
                        pattern = pattern,
                        all.files = all.files,
                        full.names = full.names,
                        recursive = recursive,
                        ignore.case = ignore.case,
                        include.dirs = include.dirs,
                        no.. = no..)
    if (exclude_symlinks) {
      # remove symlinks since such linked folders should never be considered
      # in looking for local functions since these are eactually outside of
      # the current project
      files <- Filter(function(x) !nzchar(Sys.readlink(x)), files)
    }

    return(files)
  }



  dirs <- path
  final_dirs <- character()

  if (!no_excludes) {
    exclude_regex <- paste(escape_strings(paste0(.Platform$file.sep,
                                                 exclude_folders)),
                           collapse = "|")
  }

  while (length(dirs) > 0) {
    # list all directories
    # it is faster to first exclude unwanted folders like renv right away
    # in case they are a direct subfolder of the root directory.
    sub_dirs <- lapply(dirs,
                       list.dirs,
                       full.names = TRUE,
                       recursive = FALSE)

    final_dirs <- c(final_dirs, dirs)
    dirs <- unlist(sub_dirs, use.names = FALSE)

    if (exclude_symlinks) {
      # remove symlinks since such linked folders should never be considered
      # in looking for local functions since these are eactually outside of
      # the current project
      dirs <- Filter(function(x) !nzchar(Sys.readlink(x)), dirs)
    }

    # exclude folders, usually package handler and test folders
    if (!no_excludes) {
      dirs <-
        dirs[!grepl(x = dirs,
                    pattern = exclude_regex)]
    }

  }

  # list matching files in each folder
  files <- unlist(lapply(X   = final_dirs,
                         FUN = list.files,
                         full.names   = full.names,
                         all.files    = all.files,
                         include.dirs = FALSE,
                         recursive    = FALSE,
                         patter       = pattern,
                         ignore.case  = ignore.case))

  if (include.dirs) {
    files <- sort(c(files, final_dirs))
  }

  return(files)
}
