# Begin Exclude Linting
#' @title Check which packages are actually used in a project
#' 
#' @description Provide a folder and a vector of package names to check, 
#' which packages are actually in use and which functions are used but not 
#' exported by the provided packages. 
#'
#' @param path a character vector of full path names; the default corresponds 
#' to the working directory, \link[base]{getwd}()
#' @template pkgs 
#' @param recursive logical. Should the listing recurse into directories?
#' @param exclude_files a character vector of file paths that should be 
#'  excluded from being checked Helpful if all but a few files should be
#'  considered by origin.
#' @param path_to_local_functions file path. Helpful if all project specific
#'  functions are defined in a specific folder. This folder might not be a 
#'  sub directory of the current project so the default to just find all 
#'  function definitions would not be sufficient.
#' @template check_local_conflicts 
#' @template use_markers
#'
#' @return `data.frame` invisibly, It consists of 5 columns. 
#' - `pkg`: the package that exports this function
#' - `fun`: all functions in alphabetical order
#' - `n_calls`: how often the function has been used in the files
#' - `namespaced`: logical, whether the function has been called explicitly 
#'                 via `pkg::fct` or implicitly by an attached package
#' - `conflict`: whether this function is exported by multiple checked packages
#' - `conflict_pkgs`: in case of a conflict, which packages does 
#'                   export the same function but are masked
#'  Note that functions for that it is unknown from which package they are 
#'  exported have an `NA` in the `pkg` column.
#'  Similarly, Packages that are checked but no functions from these are
#'  used are listed but have an `NA` in the `fun` column
#' @export
#'
#' @examples
#' \dontrun{
#' check_pkg_usage()
#' }
check_pkg_usage <- function(path = getwd(),
                            pkgs = getOption("origin.pkgs", .packages()),
                            recursive = TRUE,
                            exclude_files = NULL,
                            path_to_local_functions = NULL,
                            check_local_conflicts = TRUE,
                            use_markers = TRUE) {
  
  files <- list_files(path = path,
                      exclude_folders = c("renv", "packrat",
                                          ".git", ".Rproj"),
                      full.names = TRUE,
                      include.dirs = FALSE,
                      recursive = recursive,
                      pattern = "\\.R$",
                      ignore.case = TRUE)
  
  if (length(files) == 0) {
    stop("No R files in ", path)
  }
  
  
  # TODO: non absolute paths
  if (!is.null(exclude_files)) {
    if (any(!exclude_files %in% files)) {
      stop("File to exclude not in given path\n",
           exclude_files[!exclude_files %in% files])
    }
    files <- files[!files %in% exclude_files]
    
    if (length(files) == 0) {
      stop(paste("All R files excluded in", path))
    }
  }
  
  # read file
  scripts <- suppressWarnings(lapply(files, readLines))
  
  # check for empty scripts
  empty_scripts <- vapply(X = scripts,
                          FUN = length,
                          FUN.VALUE = integer(1)) == 0
  
  if (all(empty_scripts)) {
    message("All provided scripts are empty")
    return(invisible(NULL))
  } else if (any(empty_scripts)) {
    scripts <- scripts[!empty_scripts]
    files <- files[!empty_scripts]
  }
  
  # Parameter checks ---------------------------------------------------------
  if (!is.null(path_to_local_functions) &&
      !dir.exists(path_to_local_functions)) {
    stop(paste("Given path_to_local_functions",
               path_to_local_functions,
               "does not exist.",
               "Cannot check for local functions."))
  }
  
  # make sure no package is considered multiple times
  if (any(dup_pkgs <- duplicated(pkgs))) {
    warning("The following packages are provided more than once: ",
            paste(unique(pkgs[dup_pkgs]), collapse = ", "))
    pkgs <- unique(pkgs)
  }
  
  if (length(pkgs) == 0) {
    stop(paste("No packages specified. Please use either",
               "`options(origin.pkgs = c('pkg', ...))`",
               "or the `pkgs` argument."))
  }
  
  # add base package
  pkgs <- unique(c(pkgs, "base"))
  
  # Exclude Current Project Package 
  prjct_pkg <- get_project_pkg()
  if (!is.null(prjct_pkg)) {
    pkgs <- base::setdiff(pkgs, prjct_pkg)
  }
  
  if (any((unknown_pkgs <- !pkgs %in% rownames(utils::installed.packages())))) {
    stop(paste(sum(unknown_pkgs), "uninstalled packages:",
               paste(pkgs[unknown_pkgs], collapse = ", ")))
  }
  
  # get all exported functions from each package
  functions <- stats::setNames(object = lapply(X   = pkgs,
                                               FUN = get_exported_functions),
                               nm     = pkgs)
  
  if (length(unlist(functions)) == 0) {
    stop("Given packages do not export functions.")
  }
  
  # check if locally defined functions share names with exported functions
  # from checked packages.
  # Note that all projects R scripts are searched for function definitions
  if (check_local_conflicts) {
    
    # get root path of the current project
    if (is.null(path_to_local_functions)) {
      project_path <- try(rstudioapi::getActiveProject())
      
      # In case R is not run from wihtin RStudio or origin is called from
      # within a project, inform the user and determine the root path
      # by the shared root path of all files.
      project_path_found <- TRUE
      if (inherits(project_path, "try-error")) {
        project_path_found <- FALSE
        warning(paste("RStudio not running. Hence, no project path to",
                      "search for local functions can be determined."))
      } else if (is.null(project_path)) {
        project_path_found <- FALSE
        warning(paste("origin not run from within a project.",
                      "Cannot check for local functions"))
      }
      
      # Are all checked files in the current project?
      # It is possible to originize one project from within another project
      # Then, it is unclear which local functions are to consider and
      # the check is skipped
      if (project_path_found &&
          !all(not_in_project <- startsWith(x = normalizePath(files,
                                                              winslash = "/"),
                                            prefix = project_path))
      ) {
        project_path_found <- FALSE
        warning(sprintf(paste("%s files are not in the current",
                              "project path %s.\n",
                              "Cannot check for local functions due to",
                              "unclear root directory."),
                        length(not_in_project),
                        project_path))
      }
      
    } else {
      # a directory is provided
      project_path <- path_to_local_functions
    }
    
    
    if (project_path_found &&
        !is.null(project_path) &&
        !is.na(project_path) &&
        nzchar(project_path)) {
      
      # user_defined_functions
      functions <- 
        c(list(user_defined_functions = 
                 get_local_functions(path = project_path)),
          functions)
      pkgs <- c("user_defined_functions", pkgs)
      
    }
    
  }
  
  
  if (length(unlist(functions)) == 0) {
    stop("No exported functions in given packages.")
  }
  
  script_parsed <- Reduce(f = rbind,
                          Filter(f = function(x) !is.null(x),
                                 x = lapply(X   = files,
                                            FUN = get_parsed_data)))
  script_parsed$Id <- seq_len(nrow(script_parsed))
  
  if (nrow(script_parsed) == 0) {
    stop(sprintf("All scripts in this directory are empty: %s", path))
  }
  
  rm_backticks <- function(x) {
    gsub("`", "", x)
  }
  
  df_empty_sceleton <- data.frame(pkg = character(),
                                  fun = character(), 
                                  n_calls = numeric(),
                                  namespaced = logical(),
                                  conflict = logical(),
                                  conflict_pkgs = character(),
                                  stringsAsFactors = FALSE)
  
  
  # functions that are explicitly namespaced and their corresponding package
  is_namespaced_fct <- script_parsed$usage %in% "NAMESPACED_FUNCTION_CALL"
  if (any(is_namespaced_fct)) {
    pos_namespaced_fct0 <- which(is_namespaced_fct)
    pos_namespaced_fct2 <- pos_namespaced_fct0 - 2
    namespaced_functions <- 
      script_parsed[is_namespaced_fct, "text"]
    namespaced_functions <- rm_backticks(namespaced_functions)
    
    # combine the information in a data frame
    df_namespaced_funs <- 
      data.frame(pkg = script_parsed[pos_namespaced_fct2, "text"],
                 fun = namespaced_functions, 
                 stringsAsFactors = FALSE)
    # helper variable to get summary statistics
    df_namespaced_funs$n_calls <- 1
    
    df_namespaced_funs <- stats::aggregate(n_calls ~ pkg + fun,
                                           data = df_namespaced_funs,
                                           FUN = sum)
    df_namespaced_funs$namespaced <- TRUE
    df_namespaced_funs$conflict <- FALSE
    df_namespaced_funs$conflict_pkgs <- NA_character_
    
  } else {
    # shallow data frame for easier stacking
    df_namespaced_funs <- df_empty_sceleton
  }
  
  found_functions <- 
    script_parsed[script_parsed$usage %in% "FUNCTION_CALL" |
                    script_parsed$token %in% "SPECIAL", "text"]
  found_functions <- rm_backticks(found_functions)
  all_fcts <- un_list(functions)
  
  # functions that cannot be found in given packages
  undefined_functions <- found_functions[!found_functions %in% all_fcts]
  
  # setdiff uniques values
  defined_functions <-
    found_functions[!found_functions %in% unique(undefined_functions)]
  
  if (length(defined_functions) > 0) {
    # combine the information in a data frame
    df_defined_funs <- data.frame(fun = defined_functions, 
                                  stringsAsFactors = FALSE)
    # helper variable to get summary statistics
    df_defined_funs$n_calls <- 1
    
    df_defined_funs <- stats::aggregate(n_calls ~ fun,
                                        data = df_defined_funs,
                                        FUN = sum)
    
    pkg_source <-   lapply(X   = df_defined_funs$fun, 
                           FUN = function(x) names(all_fcts[all_fcts == x]))
    df_defined_funs$pkg <- unlist(lapply(X   = pkg_source, 
                                         FUN = `[[`, 
                                         1))
    df_defined_funs$namespaced <- FALSE
    df_defined_funs$conflict <- lapply(X   = pkg_source, 
                                       FUN = length) > 1
    df_defined_funs$conflict_pkgs <- NA_character_
    df_defined_funs$conflict_pkgs[df_defined_funs$conflict] <- 
      lapply(X   = lapply(X   = pkg_source[df_defined_funs$conflict],
                          FUN = `[`, 
                          -1),
             FUN = paste,
             collapse = ", ")
  } else {
    df_defined_funs <- df_empty_sceleton
  }
  
  if (nrow(df_namespaced_funs) > 0) {
    other_used_pkgs <- sort(setdiff(df_namespaced_funs$pkg, pkgs))
  } else {
    other_used_pkgs <- character(0)
  }
  
  
  used_pkgs <- setdiff(c(df_namespaced_funs$pkg, df_defined_funs$pkg),
                       c("stats", "graphics", "grDevices",
                         "datasets", "utils", "methods", "base",
                         "user_defined_functions"))
  used_pkgs <- intersect(used_pkgs, pkgs)
  unused_packages <- setdiff(pkgs, used_pkgs)
  unused_packages <- setdiff(unused_packages,
                             c("stats", "graphics", "grDevices",
                               "datasets", "utils", "methods", "base",
                               "user_defined_functions"))
  
  if (length(unused_packages) > 0) {
    df_unused_packages <- data.frame(pkg = sort(unused_packages),
                                     fun = NA_character_,
                                     n_calls = 0,
                                     namespaced = NA,
                                     conflict = NA,
                                     conflict_pkgs = NA_character_,
                                     stringsAsFactors = FALSE)
  } else {
    df_unused_packages <- df_empty_sceleton
  }
  
  
  if (length(undefined_functions) > 0) {
    undefined_funs_tbl <- table(undefined_functions)
    df_undefined_funs <- data.frame(pkg = NA_character_,
                                    fun = names(undefined_funs_tbl),
                                    n_calls = as.numeric(undefined_funs_tbl),
                                    namespaced = FALSE,
                                    conflict = NA,
                                    conflict_pkgs = NA_character_,
                                    stringsAsFactors = FALSE)
  } else {
    df_undefined_funs <- df_empty_sceleton
  }
  
  
  
  if (use_markers) {
    df_logging <- script_parsed
    
    df_logging$log_type <- ""
    
    if (nrow(df_namespaced_funs) > 0) {
      # mark functions that are not in given packages but namespaced
      to_insert <- 
        df_logging$text %in% 
        df_namespaced_funs[!df_namespaced_funs$pkg %in% pkgs, "fun"] &
        df_logging$usage %in% "NAMESPACED_FUNCTION_CALL"
      
      # to highlight the full namespaced call (`pkg::fct`), put them together
      # in a single line. For this, the following tweak is needed
      if (any(to_insert)) {
        # mark positions
        to_insert_pos0 <- which(to_insert)
        to_insert_pos1 <- to_insert_pos0 - 1
        to_insert_pos2 <- to_insert_pos0 - 2
        
        # paste the `pkg::fct` together
        df_logging[to_insert_pos0, "text"] <- 
          paste0(df_logging[to_insert_pos2, "text"],
                 df_logging[to_insert_pos1, "text"],
                 df_logging[to_insert_pos0, "text"])
        # take the start of the call the sart of `pkg`
        df_logging[to_insert_pos0, "col1"] <- 
          df_logging[to_insert_pos2, "col1"]
        # use color highlighting for insertions
        df_logging[to_insert,
                   "log_type"] <- "INSERT"
        # remove merged rows
        df_logging <- df_logging[-c(to_insert_pos2, to_insert_pos1), ]
        
      }
    }
    
    # mark functions that are not in given packages and NOT namespaced, hence 
    # their origin is unknown
    df_logging[df_logging$text %in% undefined_functions &
                 df_logging$usage %in% "FUNCTION_CALL",
               "log_type"] <- "MISSING"
    
    df_logging$pkg_nchar <- 0
    # lines that are relevant for logging
    logging_data <- make_logging_data(df_logging,
                                      use_markers = use_markers,
                                      type_fun = "check")
    
    if (!is.null(logging_data) &&
        nrow(logging_data) > 0 &&
        interactive()) {
      # nocov start
      rstudioapi::sourceMarkers(name = "origin - Function and Package Usage",
                                markers = logging_data) 
      # nocov end
    }
  }
  
  # prepare data for output --------------------------
  # combine all data sources
  df_out <- rbind(df_namespaced_funs, df_defined_funs)
  df_out <- df_out[order(df_out$pkg, df_out$fun, df_out$namespaced), ]
  df_out <- rbind(df_out, df_undefined_funs, df_unused_packages)
  rownames(df_out) <- NULL
  
  # attach checked packages via an attribute
  attr(df_out, "pkgs") <- pkgs
  
  # give specific class for printing
  class(df_out) <- c("pkg_usage", class(df_out))
  
  
  return(df_out)
  
}
# End Exclude Linting
