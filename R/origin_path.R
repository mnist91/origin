#' Add Explicit Package Names to its Functions
#'
#' @param file
#' @param pkgs
#' @param overwrite
#' @param ignoreComments
#' @param excludeBasePackages
#' @param verbose
#'
#' @return
#' @export
#'
#' @examples
addPackageToFunction_all <- function(file,
                                     pkgs = .packages(),
                                     overwrite = FALSE,
                                     ignoreComments = TRUE,
                                     excludeBasePackages = TRUE,
                                     verbose = FALSE) {

  if(exists("lg")){
    dbaHelpers::lgDifftime()
  }

  if(!file.exists(file)) {
    stop("No file in this path\n", file)
  }

  # Lese file ein
  script <- readLines(file)


  # exkludiere R-base Packages
  if(excludeBasePackages) {
    pkgs <- setdiff(pkgs, c("stats", "graphics", "grDevices", "datasets",
                            "utils", "methods", "base"))
  }

  functions <- lapply(X = pkgs, FUN = getFunctions)


  # DUPLICATES ---------------------------------------------------------------
  # finde Funktionen, die in mehreren Paketen vorkommen.
  # Eine zuordnung ist hier nicht eineindeutig möglich
  functions <- setNames(functions, pkgs)

  # Paket mit den meisten Funtionen, wichtig für purrr::transpose()
  longestList <- which.max(lapply(functions, length))

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

  # which dupplilcates are in the script
  dup_funs_in_script <- sapply(names(dups),
                               function(FUN) {
                                 grepl(pattern = FUN,
                                       x = paste(script, collapse = ""),
                                       fixed = TRUE)
                               })

  # Require User interaction if cuplicates are detected
  if(any(dup_funs_in_script)) {
    crayon_danger <- crayon::combine_styles(crayon::red, crayon::underline, crayon::bold)
    cat(crayon_danger("Used functions in mutliple Packages!"), "\n")
    cat(paste(dups[dup_funs_in_script], ": ", names(dups[dup_funs_in_script]),
              collapse = "\n", sep = ""),
        "\n")
    cat("Order in which packges are evaluated;\n\n")
    cat(paste(pkgs[pkgs %in% names(funs_duplicates)], collapse = " >> "), "\n")

    cat("Do you want to proceed?\n")
    if(interactive()) {
      answer <- menu(choices = c("YES", "NO"))
    } else {
      answer <- 1
    }
    if(answer != 1) {
      stop("Execution halted")
    }


  }

  # get relevant function informations
  l <- checkFunctions(script = script,
                      functions = funs_unlisted,
                      verbose = verbose)

  # iterate over all functions and add package:: when necessary
  purrr::walk2(.x = pkgs,
               .y = functions,
               .f = ~ addPackageToFunction(pkg            = .x,
                                           functions      = .y,
                                           file           = file,
                                           overwrite      = overwrite,
                                           ignoreComments = ignoreComments,
                                           verbose        = verbose)
  )


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

