#' Add Explicit Package Names to its Functions
#'
#' @param file a path to a script
#' @param pkgs a vector with package names
#' @param overwrite a boolean, if TRUE the file will be saved and overwritten. If FALSE the file is returned.
#' @param ignoreComments a boolean, if TRUE lines starting with # are ignored
#' @param excludeBasePackages a boolean, if TRUE base R functions are excluded
#' @param verbose a boolean
#'
#' @return
#' @export
#'
addPackageToFunction_all <- function(file,
                                     pkgs = .packages(),
                                     overwrite = FALSE,
                                     ignoreComments = TRUE,
                                     excludeBasePackages = TRUE,
                                     verbose = FALSE) {

  if (!file.exists(file)) {
    stop("No file in this path\n", file)
  }

  # read file
  script <- readLines(file)


  # exclude base R packages
  if (excludeBasePackages) {
    pkgs <- setdiff(pkgs, c("stats", "graphics", "grDevices", "datasets",
                            "utils", "methods", "base"))
  }

  functions <- lapply(X = pkgs, FUN = getFunctions)


  # DUPLICATES ---------------------------------------------------------------
  # find functions, that are called within multiple packages
  # a automatic assignment is not possible in such cases
  # a deterministic order is chosen
  functions <- setNames(functions, pkgs)


  # named character vector of functions with package name as names
  funs_unlisted <- unlist(
    unname(
      Map(function(x, nm) {
        setNames(x, rep(nm, length(x)))
      },
      functions,
      names(functions)
      )
    )
  )


  funs_duplicates <- funs_unlisted[which(duplicated(funs_unlisted))]
  funs_duplicates <- funs_unlisted[funs_unlisted %in% funs_duplicates]

  # duplicate functions and their corresponding packages
  dups <- by(names(funs_duplicates), funs_duplicates, paste, collapse = ", ")

  # which duplicates are in the script
  dup_funs_in_script <- sapply(names(dups),
                               function(FUN) {
                                 grepl(pattern = FUN,
                                       x = paste(script, collapse = ""),
                                       fixed = TRUE)
                               })

  # Require User interaction if duplicates are detected
  if (any(dup_funs_in_script)) {
    crayon_danger <- crayon::combine_styles(crayon::red,
                                            crayon::underline,
                                            crayon::bold)
    cat(crayon_danger("Used functions in mutliple Packages!"), "\n")
    cat(paste(dups[dup_funs_in_script], ": ", names(dups[dup_funs_in_script]),
              collapse = "\n", sep = ""),
        "\n")
    cat("Order in which packges are evaluated;\n\n")
    cat(paste(pkgs[pkgs %in% names(funs_duplicates)], collapse = " >> "), "\n")

    cat("Do you want to proceed?\n")
    if (interactive()) {
      answer <- menu(choices = c("YES", "NO"))
    } else {
      answer <- 1
    }
    if (answer != 1) {
      stop("Execution halted")
    }


  }

  # get relevant function information
  l <- checkFunctions(script = script,
                      functions = funs_unlisted,
                      verbose = verbose,
                      ignoreComments = ignoreComments)

  # iterate over all functions and add package:: when necessary
  invisible(
    Map(f = function(pkg, funs) {
      addPackageToFunction(pkg            = pkg,
                           functions      = funs,
                           file           = file,
                           overwrite      = overwrite,
                           ignoreComments = ignoreComments,
                           verbose        = verbose)
    },
    pkgs,
    functions
    ))


  # Check all changes and potentially missed functions
  script_after <- readLines(file)
  verbolize(script_prior = script,
            script_after = script_after,
            lineMatches = l$lineMatches,
            functions = funs_unlisted,
            functionsInScript = l$functionsInScript,
            special_functions = l$special_functions,
            special_matches = l$special_matches)

}
