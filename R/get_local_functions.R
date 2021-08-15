#' Find All User Defined functions in the Project
#'
#' @param path Path in which all defined function names should be
#'  found and retrieved. Defaults to the current working directory.
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



  # In case that the project folder is very deep, for example because a huge
  # share file sevrver is linked in the project folder, a timeout is triggered
  time_limit <- 180
  setTimeLimit(cpu = time_limit, elapsed = time_limit, transient = TRUE)
  on.exit({
    setTimeLimit(cpu = Inf, elapsed = Inf, transient = FALSE)
  })

  tryCatch({
    # all R files that do not live in the foldrs renv/packrat/tests
    files <- list_files(path = root,
                        exclude_folders = c("renv", "packrat", "tests"),
                        full.names = TRUE,
                        include.dirs = FALSE,
                        recursive = TRUE,
                        pattern = "\\.R$",
                        ignore.case = TRUE)

  }, error = function(e) {
    if (grepl("time limit", e$message)) {
      stop(paste(e$message,
                 "when searching all files for function definitions.",
                 "Consider setting",
                 "options(origin.check_local_conflicts = FALSE)."))
    } else {
      # error not related to timeout
      stop(e)
    }
  })


  # read files
  scripts <- suppressWarnings(lapply(X = files, FUN = readLines))

  # find funciton names
  local_funs <- sort(unique(unlist(lapply(X = scripts, FUN = find_functions))))

  return(local_funs)
}
