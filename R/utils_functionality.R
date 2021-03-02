#' Get All Exported Functions From a Package
#'
#' @param pkg `character(1)`
#'   Name of package
#'
#' @return `character(1)`
#' @export
#'
#' @examples
#' getFunctions("data.table")
#'
getFunctions <- function(pkg) {
  # get all exported functions from a package --------------------------------
  # lese alle exportierten Funktionen aus dem Paket ein
  # lists all (incl. not exported) functions
  functions <- lsf.str(paste0("package::", pkg),
                       envir = getNamespace(pkg))
  # lists all exports of a package (incl. non functions)
  exports <- getNamespaceExports(pkg)
  # overlap of exports and functions -> exported functions
  functions <- functions[functions %in% exports]

  return(functions)
}


#' Extract relevant Function information
#'
#' @param script a script to check
#' @param functions a vector with function names
#' @param ignoreComments a boolean, if TRUE lines starting with # are ignored
#'
#' @return
#' @export
#'
checkFunctions <- function(script,
                           functions,
                           ignoreComments = TRUE,
                           pkg = NULL,
                           verbose = TRUE) {
  # remove function with special characters like %, &, [] and :
  special_functions <- grepl(functions, pattern = "\\&|\\:|\\]|\\[|%")
  relevant_functions <- functions[!special_functions]
  # reduce the number of functions to check, by selecting possible (occurring)
  # functions. This isn't a check if it is a function or an object,
  # but a simple regular expression

  # TODO: check performance of one script vs multiple scripts
  # any(grepl(pattern = .x, x = script, fixed = TRUE))
  fullScript <- paste0(script, collapse = "")

  matches <- purrr::map_lgl(relevant_functions,
                            ~grepl(pattern = .x, x = fullScript, fixed = TRUE))
  functionsInScript <- relevant_functions[matches]

  # special functions such as %like" can not be called with ::
  # print a warning, that such functions occur
  special_matches <- purrr::map_lgl(functions[special_functions],
                                    ~grepl(pattern = .x,
                                           x = fullScript,
                                           fixed = TRUE))

  # no matching functions
  if (!any(matches)) {
    if (!verbose) {
      return(NULL)
    }
    if (!is.null(pkg)) {
      cat(crayon::red(pkg, paste(rep("-", 100 - nchar(pkg)),
                                 collapse = "")), "\n")
    }
    cat(crayon::green(0, "Lines changed\n"))
    cat(crayon::green(0, "Functions recognized\n"))

    if (any(special_matches)) {
      special_functions_in_script <-
        functions[special_functions][special_matches]
      cat(crayon::magenta("Special Functions used!"), "\n")
      specialMatches <- which(as.logical(
        Reduce(f = "+", purrr::map(.x = special_functions_in_script,
                                   .f = ~grepl(x = script,
                                               pattern = .x, fixed = TRUE)))
        ))
      cat(paste(paste("Line ", specialMatches, ": ",
                      script[specialMatches], sep = ""),
                collapse = "\n"))
    }
    cat("\n")
    return(NULL)
  }

  # reduce the number of script rows to check, by selecting only those which
  # contain a function name
  lineMatches <- grepl(x = script,
                       pattern = paste(functionsInScript, collapse = "|"))

  # ignore comment rows
  if (ignoreComments) {
    # starts with # or leading spaces and #
    # startsWithHashRegEx <- "(?<=^[ *]|^)#"
    lineComments <- grepl(x = trimws(script[lineMatches]), pattern = "^#")

    # ignore these line for the matching
    lineCommentsMatches <- which(lineMatches)[which(lineComments)]
    lineMatches[lineCommentsMatches] <- FALSE

  }


  return(list(matches = matches,
              lineMatches = lineMatches,
              special_matches = special_matches,
              special_functions = special_functions,
              functionsInScript = functionsInScript))

}
