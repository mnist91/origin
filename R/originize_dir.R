#' Originize a complete directory
#'
#' @description To originize complete folders/projects, this function finds
#' and originizes all R files within this folder and
#' (by default) its subdirectories.
#'
#'
#' @details check_conflicts checks whether multiple packages listed in pkgs
#' export
#' functions with the same name, e.g. lag() is both part of the dplyr and
#' data.table namespace. If there are any conflicts actually present
#' in any considered script, these conflicts are shown including how origin
#' would solve them. User input is required to proceed. The order in pkgs
#' determines the precedence, while those listed first have higher precedence
#' than those listed later in the vector. This is consistent with function
#' masking in R.
#'
#' check_base_conflicts checks whether functions listed in pkgs mask R functions
#'  of R core packages (base, utils, stats, methods, graphics, grDevices,
#'  datasets). Even tough the user might not include those functions in the
#'  pkg::fct logic, potential conflicts require careful evaluation.
#'
#' @param path path to a directory. Defaults to the current working directory.
#' @template pkgs
#' @param recursive logical. Should scripts be originized recursively, this
#'  means that all files in the subfolders will be searched as well.
#'  See \link[base]{list.files}
#' @param exclude_files a character vector of file paths that should be excluded
#' excluded from being originized. Helpful if all but a few files should be
#' considered by origin.
#' @template overwrite
#' @template ask_before_applying_changes
#' @template check_conflicts
#' @template check_base_conflicts
#' @template check_local_conflicts
#' @template path_to_local_functions
#' @template add_base_packages
#' @template excluded_functions
#' @template verbose
#' @template use_markers
#' @template filetypes
#'
#' @return No return value, called for side effects
#'
#' @export
#'
#' @examples
#' \dontrun{
#' originize_dir(path = "folder_to_originize",
#'               pkgs = c("dplyr", "data.table"),
#'               overwrite = TRUE,
#'               ask_before_applying_changes = TRUE,
#'               excluded_functions = list(dplyr = c("%>%", "tibble"),
#'                                         data.table = c(":=", "%like%"),
#'                                         # generally exclude
#'                                         c("last", "first")),
#'               exclude_files = c("dont_originize_this.R",
#'                                 "dont_originize_that.R"),
#'               filetypes = c(".R", ".rmd", ".qmd"),
#'               verbose = TRUE)
#' }
originize_dir <-
  function(
    path = getwd(),
    pkgs = getOption("origin.pkgs", .packages()),
    recursive = TRUE,
    exclude_files = NULL,
    overwrite =
      getOption("origin.overwrite", TRUE),
    ask_before_applying_changes =
      getOption("origin.ask_before_applying_changes", TRUE),
    check_conflicts =
      getOption("origin.check_conflicts", TRUE),
    check_base_conflicts =
      getOption("origin.check_base_conflicts", TRUE),
    path_to_local_functions =
      getOption("origin.path_to_local_functions", NULL),
    check_local_conflicts =
      getOption("origin.check_local_conflicts", TRUE),
    add_base_packages =
      getOption("origin.add_base_packages", FALSE),
    excluded_functions =
      getOption("origin.excluded_functions", list()),
    verbose =
      getOption("origin.verbose", FALSE),
    use_markers =
      getOption("origin.use_markers_for_logging", TRUE),
    filetypes =
      getOption("origin.filetypes", "R")
  ) {
    
    if (!check_base_conflicts && add_base_packages) {
      stop("When adding base packages checking for ",
           "potential conflicts is required!")
    }
    
    # make regex from requested filetypes --------------------------------------
    # remove point rather than add it to make standardizing process easier
    filetype_pattern <- make_filetype_pattern(filetypes)
    
    # list files to originize --------------------------------------------------
    files <- list_files(path = path,
                        exclude_folders = c("renv", "packrat",
                                            ".git", ".Rproj"),
                        full.names = TRUE,
                        include.dirs = FALSE,
                        recursive = recursive,
                        pattern = filetype_pattern,
                        ignore.case = TRUE)
    
    
    # TODO: non absolute paths
    if (!is.null(exclude_files)) {
      if (any(!exclude_files %in% files)) {
        stop("File to exclude not in given path\n",
             exclude_files[!exclude_files %in% files])
      }
      files <- files[!files %in% exclude_files]
    }
    
    
    # warning if many files are about to be originized
    n_files <- length(files)
    if (n_files > 20) {
      if (interactive() && ask_before_applying_changes) {
        # nocov start
        cat(sprintf("You are about to originize %s files.\nProceed?", n_files))
        answer <- utils::menu(choices = c("YES", "NO", "Show files"))
        # nocov end
      } else {
        answer <- 1
      }
      
      if (answer == 2) {
        # nocov start
        stop("Execution halted")
      } else if (answer == 3) {
        print(files)
        cat("\nProceed?")
        answer2 <- utils::menu(choices = c("YES", "NO"))
        
        if (answer2 == 2) {
          stop("Execution halted")
        }
      }
      # nocov end
    }
    
    # read file
    scripts <- suppressWarnings(lapply(files, readLines))
    
    # check for empty scripts
    empty_scripts <- vapply(X = scripts,
                            FUN = function(x) {
                              length(x) == 0 || all(!nzchar(x))
                            },
                            FUN.VALUE = logical(1))
    
    if (all(empty_scripts)) {
      message("All provided scripts are empty")
      return(invisible(NULL))
    } else if (any(empty_scripts)) {
      scripts <- scripts[!empty_scripts]
      files <- files[!empty_scripts]
    }
    
    scripts_clean <- Map(f = function(f, s) {
      if (is_rmd_file(f)) {
        return(extract_r_chunks(s))
      } else {
        return(s)
      }
    }, files, scripts)
    
    originize_wrap(scripts = scripts,
                   scripts_clean = scripts_clean,
                   files = files,
                   type = "writeLines",
                   pkgs = pkgs,
                   overwrite = overwrite,
                   ask_before_applying_changes = ask_before_applying_changes,
                   check_conflicts = check_conflicts,
                   check_base_conflicts = check_base_conflicts,
                   add_base_packages = add_base_packages,
                   excluded_functions = excluded_functions,
                   verbose = verbose,
                   use_markers = use_markers,
                   path_to_local_functions = path_to_local_functions,
                   check_local_conflicts = check_local_conflicts)
    
    return(invisible(NULL))
    
  }
