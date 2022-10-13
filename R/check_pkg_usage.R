check_pkg_usage <- function(pkgs = getOption("origin.pkgs", .packages()),
                            path = getwd(),
                            recursive = TRUE,
                            exclude_files = NULL,
                            path_to_local_functions = NULL,
                            check_local_conflicts = TRUE,
                            ignore_comments = TRUE,
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
      stop("All R files excluded", path)
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

      # user defined functions
      functions[["user defined functions"]] <-
        get_local_functions(path = project_path)
      pkgs <- c(pkgs, "user defined functions")

    }

  }


  if (length(unlist(functions)) == 0) {
    stop("No exported functions in given packages.")
  }

  if (ignore_comments) {
    scripts <- lapply(scripts, function(s) {
      gsub("^\\s*#.*", "", x = s)
    })
  }

  # in case this full script collapsing has not happend earlier.
  # better performance if an error is triggered prior to this step
  if (!exists("script_collapsed")) {
    script_collapsed <- paste(lapply(X = scripts,
                                     FUN = paste,
                                     collapse = ""),
                              collapse = "")
  }

  string_before_function <-
    "(?<=[[:blank:],;=&/\\-<>~!\\|\\?\\*\\^\\+\\(\\[\\{(:{2})]|^)"
  function_regex <-
    paste0(string_before_function,
           "(:* *(([[:alpha:]]|[.][._[:alpha:]])[._[:alnum:]]*)|[.])\\s*\\(")

  matches <- get_matches(regex = function_regex,
                         line = 1,
                         text = script_collapsed,
                         perl = TRUE,
                         fixed = FALSE)
  # vector of found functions
  found_functions <-
    Map(f = function(x, start, len) {
      substr(x = rep(x, length(start)),
             start = start,
             stop = start + len - 1)
    },
    matches$string,
    matches$matches,
    matches$log_length)[[1]]

  # unspecified functionas only
  found_functions <- found_functions[!grepl("^:", found_functions)]
  # remove everything up from and starting with the opening bracket
  found_functions <- gsub("\\(*", "", found_functions)
  # remove sorrounding white space
  found_functions <- trimws(found_functions)

  found_functions_unique <- sort(unique(found_functions))

  undefined_functions <- setdiff(found_functions_unique, unlist(functions))

  found_functions_count <- table(found_functions[found_functions %in%
                                                   undefined_functions])
  valid_r_object_name <- "((([[:alpha:]]|[.][._[:alpha:]])[._[:alnum:]]*)|[.])"
  regex <- paste0(valid_r_object_name, "\\s*::+\\s*", valid_r_object_name)
  empty_list <- list(line = integer(),
                     string = character(),
                     matches = list(),
                     log_length = list(),
                     type = character())

  escape_html <- function(x) {
    if (any(is_html_line <- grepl(pattern = "<|>", x = x))) {
      x[is_html_line] <-
        gsub(pattern     = ">",
             replacement = "&gt;",
             x           = gsub(pattern     = "<",
                                replacement = "&lt;",
                                x           = x[is_html_line]))
    }
    return(x)
  }


  logging_data <- Map(
    f = function(file, script) {

      miss_fcts <- FALSE
      miss_pkgs <- FALSE
      # get relevant function information
      # TODO: still relevant / much time improvement here?
      l <- check_functions(script = script,
                           functions = undefined_functions,
                           # Comments are already excluded here
                           ignore_comments = FALSE)
      # make function-Output available
      matches <- l$matches
      line_matches <- l$line_matches
      functions_in_script <- l$functions_in_script

      pkg_lines <- grepl("::", x = script, fixed = TRUE)
      pkg_matches <- empty_list
      regular_calls <- empty_list
      pkg_nms <- character()

      if (any(pkg_lines)) {
        pkg_matches <- get_matches(text = script[pkg_lines],
                                   regex = regex,
                                   line = which(pkg_lines),
                                   perl = TRUE,
                                   fixed = FALSE,
                                   filter_nomatches = TRUE)

        # todo: den schritt nur einzeln machen, um die paketinfo zu behalten
        pkg_nms <- Map(
          f = function(x, start, len) {
            out <- substr(x = rep(x, length(start)),
                          start = start,
                          stop = start + len - 1)
            trimws(gsub("::+.*", "", out))
          },
          pkg_matches$string,
          pkg_matches$matches,
          pkg_matches$log_length)
        valid_pkg <- lapply(pkg_nms, Negate(`%in%`), pkgs)

        any_valid <- unlist(lapply(valid_pkg, any))

        if (any(any_valid)) {
          valid_pkg <- valid_pkg[any_valid]
          miss_pkgs <- TRUE
          pkg_matches$line <- pkg_matches$line[any_valid]
          pkg_matches$string <- pkg_matches$string[any_valid]
          pkg_matches$matches <- Map(f = function(x, y) x[y],
                                     pkg_matches$matches[any_valid],
                                     valid_pkg)
          pkg_matches$log_length <- Map(f = function(x, y) x[y],
                                        pkg_matches$log_length[any_valid],
                                        valid_pkg)
          pkg_matches$type <- rep("INSERT", length(pkg_matches$log_length))
        } else {
          pkg_matches <- empty_list
        }
      }

      if (any(matches)) {
        funs_prep <- paste0(escape_strings(functions_in_script), collapse = "|")

        # tokens that can occur right before a function calls
        # Begin Exclude Linting
        # pre_fun_tokens <- c(",", ";", "=", "&", "/", "-",
        #                     "<", ">", "~", "!", "|",
        #                     "?", "*", "^", "+", "(", "[", "{")
        # paste(escape_strings(pre_fun_tokens), collapse = "")
        # End Exclude Linting
        pattern_regex <-
          paste0("(?<=[[:blank:],;=&/\\-<>~!\\|\\?\\*\\^\\+\\(\\[\\{]|^)(",
                 funs_prep,
                 ") *\\(")

        regular_calls <- get_matches(script[line_matches],
                                     line = which(line_matches),
                                     regex = pattern_regex,
                                     perl = TRUE,
                                     filter_nomatches = TRUE)
        if (length(regular_calls$matches) > 0) {

          # Aufgehende Klammer nicht highlighten
          regular_calls$log_length <- lapply(regular_calls$log_length, `-`, 1)
          regular_calls$type <- rep("MISSING", length(regular_calls$log_length))
          miss_fcts <- TRUE
        } else {
          regular_calls <- empty_list
        }
      } else {
        regular_calls <- empty_list
      }

      if (!miss_fcts && !miss_pkgs) {
        return(NULL)
      }

      # combine positions of potential missings
      logging_comb <- Map(f = c,
                          pkg_matches,
                          regular_calls)
      # prepare strings and insert color highlighting where needed
      logging_data <- lapply(
        X = sort(unique(logging_comb$line)),
        FUN = prep_line_logging,
        lines = logging_comb$line,
        matches = logging_comb$matches,
        pkg = "undefined",
        log_length = logging_comb$log_length,
        type = logging_comb$type,
        string = logging_comb$string,
        use_markers = use_markers)
      # combine all lines
      logging_data <- Reduce(rbind, logging_data)

      if (use_markers && !is.null(logging_data)) {
        attr(logging_data$message, which = "class") <- c("html", "character")
      }

      logging_data$file <- file
      return(list(logging_data = logging_data,
                  pkg_nms = unlist(pkg_nms)))


    },
    files,
    if (use_markers) lapply(X = scripts, FUN = escape_html) else scripts,
    USE.NAMES = FALSE
  )

  other_used_pkgs <- table(unlist(lapply(X = logging_data,
                                         FUN = `[[`,
                                         "pkg_nms")))
  other_used_pkgs <- other_used_pkgs[!names(other_used_pkgs) %in% pkgs]
  other_used_pkgs <- other_used_pkgs[sort(names(other_used_pkgs))]


  if (use_markers) {
    logging_data_missings <- Reduce(f = rbind,
                                    x = Filter(f = function(x)  length(x) > 0,
                                               x = lapply(X = logging_data,
                                                          FUN = `[[`,
                                                          "logging_data")))
    if (!is.null(logging_data_missings) && nrow(logging_data_missings) > 0) {
      rstudioapi::sourceMarkers(name = "origin - Function and Package Usage",
                                markers = logging_data_missings)
    }
  }

  # reduce checked functions to matched names in any part of any script
  functions <- lapply(functions,
                      FUN = function(funs) {
                        funs[vapply(X = funs,
                                    FUN = function(f) {
                                      grepl(pattern = f,
                                            x = script_collapsed,
                                            fixed = TRUE)
                                    },
                                    FUN.VALUE = logical(1),
                                    USE.NAMES = TRUE)]
                      })

  # iterate over all functions and find position where package:: is necessary
  result_list <-
    Map(f = function(pkg, funs) {
      get_origins(pkg             = pkg,
                  script          = script_collapsed,
                  functions       = funs,
                  specific        = TRUE)
    },
    pkgs,
    functions
    )

  # keep package info only if matches are present
  result_list <- Filter(function(l) length(l) > 0, result_list)

  # combine results for all packages
  combined <- Reduce(f = function(...) {
    Map(f = c,
        ...)
  },
  x = result_list)


  # vector of found functions
  found_functions <- lapply(result_list,
                            FUN = function(rl) {
                              funs <- Map(
                                f = function(x, start, len) {
                                  substr(x = rep(x, length(start)),
                                         start = start,
                                         stop = start + len - 1)
                                },
                                rl$string,
                                rl$matches,
                                rl$match_length)
                              return(unlist(funs, use.names = FALSE))
                            })
  found_functions <- Filter(f = function(x) !is.null(x),
                            x = found_functions)
  res <- un_list(found_functions)

  # remove everything up from and starting with the opening bracket
  res <- gsub("\\(*", "", res)
  # remove leading package specification
  res <- gsub("^.*::*", "", res)
  # remove sorrounding white space
  res <- trimws(res)

  if (length(res) > 0) {
    result <- data.frame(pkg = names(res),
                         fun = res,
                         stringsAsFactors = FALSE)
    # helper variable to get summary statistics
    result$x <- rep(1, nrow(result))

    out <- stats::aggregate(x ~ pkg + fun,
                            data = result,
                            FUN = sum)
    names(out) <- c("pkg", "fun", "n_calls")
    out$conflict <- duplicated_all(out$fun)

    mult_matches <- stats::aggregate(pkg ~ fun,
                                     data = out,
                                     FUN = paste,
                                     collapse = ", ")
    names(mult_matches) <- c("fun", "conflict_pkgs")
    out2 <- merge.data.frame(x = out,
                             y = mult_matches,
                             by = "fun",
                             all.x = TRUE)
    unique_source <- out2$pkg != out2$conflict_pkgs
    out2[!unique_source, "conflict_pkgs"] <- NA_character_
    out2[unique_source, "conflict_pkgs"] <-
      mapply(FUN = function(x, y) gsub(x, "", y),
             out2[unique_source, "pkg"],
             out2[unique_source, "conflict_pkgs"])
    out2[unique_source, "conflict_pkgs"] <-
      gsub(pattern = "^, |, $",
           replacement = "",
           x = out2[unique_source, "conflict_pkgs"])
  } else {
    out2 <- data.frame(pkg = character(),
                       fun = character(),
                       n_calls = numeric(),
                       conflict = logical(),
                       conflict_pkgs = character(),
                       stringsAsFactors = FALSE)
  }


  used_pkgs <- unique(out2$pkg)
  used_pkgs <- setdiff(used_pkgs,
                       c("stats", "graphics", "grDevices",
                         "datasets", "utils", "methods", "base",
                         "user defined functions"))
  if (length(used_pkgs) > 0) {
    cat("Used Packages:", length(used_pkgs), "\n\n",
        paste(used_pkgs, collapse = ", "), "\n\n")
  }

  unused_packages <- sort(setdiff(pkgs, used_pkgs))
  unused_packages <- setdiff(unused_packages,
                             c("stats", "graphics", "grDevices",
                               "datasets", "utils", "methods", "base",
                               "user defined functions"))

  if (length(unused_packages) > 0) {
    unused_packages_dat <- data.frame(pkg = unused_packages,
                                      fun = NA_character_,
                                      n_calls = 0,
                                      conflict = NA,
                                      conflict_pkgs = NA_character_,
                                      stringsAsFactors = FALSE)
    out2 <- rbind(out2, unused_packages_dat)
    cat("Unused Packages:", length(unused_packages), "\n\n",
        paste(unused_packages, collapse = ", "), "\n\n")
  } else {
    cat("No unused packages!\n\n")
  }



  if (length(other_used_pkgs) > 0) {
    other_used_pkgs_dat <- data.frame(pkg = names(other_used_pkgs),
                                      fun = NA_character_,
                                      n_calls = as.numeric(other_used_pkgs),
                                      conflict = NA,
                                      conflict_pkgs = NA_character_,
                                      stringsAsFactors = FALSE)
    out2 <- rbind(out2, other_used_pkgs_dat)

    cat("Specifically (`pkg::fun()`) used Packages:", length(other_used_pkgs),
        "\n\n",
        paste(names(other_used_pkgs), collapse = ", "), "\n\n")
  }

  if (length(undefined_functions) > 0) {
    undefined_function_dat <-
      data.frame(pkg = NA_character_,
                 fun = names(found_functions_count),
                 n_calls = as.numeric(found_functions_count),
                 conflict = NA,
                 conflict_pkgs = NA_character_,
                 stringsAsFactors = FALSE)
    out2 <- rbind(out2, undefined_function_dat)
    cat("Functions with unknown origin:", length(undefined_functions), "\n\n",
        paste(undefined_functions, collapse = ", "), "\n\n")


  } else {
    cat("All used functions defined!\n")
  }

  return(invisible(out2))

}

duplicated_all <- function(x) {
  x %in% x[duplicated(x)]
}
