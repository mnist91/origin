#' Add Package Name to Function Calls
#'
#' @param pkg a package name
#' @param file a path to a script
#' @param overwrite a boolean, if TRUE the file will be saved and overwritten. If FALSE the file is returned.
#' @param ignoreComments a boolean, if TRUE lines starting with # are ignored
#' @param verbose a boolean
#' @param functions a vector with function names
#'
#' @noRd
#' @return

originize <- function(pkg,
                      file = NULL,
                      overwrite = FALSE,
                      ignoreComments = TRUE,
                      verbose = FALSE,
                      functions = NULL) {
  
  
  if (!file.exists(file)) {
    stop("No file in this path\n", file)
    # load file
  } else {
    script <- readLines(file)
  }
  
  # save prior script for comparison
  if (verbose) {
    script_prior <- script
  }
  
  if (is.null(functions)) {
    # get all exported functions from a package --------------------------------
    functions <- getFunctions(pkg)
  }
  
  l <- checkFunctions(script = script,
                      functions = functions,
                      pkg = pkg,
                      verbose = verbose)
  
  # make function-Output available
  matches <- l$matches
  lineMatches <- l$lineMatches
  special_matches <- l$special_matches
  functionsInScript <- l$functionsInScript
  special_functions <- l$special_functions
  if (!any(matches)) {
    return(NULL)
  }
  
  
  # regular expression, to prefix package name to functions
  replacementRegex <- paste0("\\1", pkg, "::\\2\\3")
  
  # regular expression, to identify usage with *apply/purrr
  funArguments <- c("FUN", "\\.f")
  assignWithSpaces <- c("=", " = ", "= ", " =")
  leadingPatterns <- paste(apply(expand.grid(funArguments, assignWithSpaces),
                                 MARGIN = 1,
                                 FUN = paste0,
                                 collapse = ""),
                           collapse = "|")
  
  script[lineMatches] <-
    # iterate over all scripts' rows
    purrr::map_chr(
      .x = script[lineMatches],
      .f = function(LINE) {
        # iterate over all functions
        # using reduce to not override prevoius changed functions
        purrr::reduce(
          .x = functionsInScript,
          .f = function(STRING, FUN) {
            
            # gsub only changes the first occurrence, hence a while loop
            # potentially changes this code part
            
            # function name must be after a comma, space or an open bracket
            # or at the beginning of a row
            # in addition it must be followed by an open round bracket
            FUN <- gsub("\\.", "\\\\.", x = FUN)
            patternRegex <- paste0("(.*)((?<=[, \\(]|^)", FUN, " *\\()(.*)")
            
            # In functional programming, the function's call is used without
            # brackets but as an object. To differentiate between such a
            # function and a variable, an explicit parameter is needed:
            # e.g. *apply "FUN = "
            # e.g. purr ".f = "
            functionalPatternRegex <-
              paste0("(.*)((?<=", leadingPatterns, ")",
                     FUN, "(?=[ \\,)]|$))(.*)")
            
            repeat{
              STRING_PRIOR <- STRING
              STRING <- gsub(x = STRING,
                             pattern = patternRegex,
                             replacement = replacementRegex,
                             perl = TRUE)
              STRING <- gsub(x = STRING,
                             pattern = functionalPatternRegex,
                             replacement = replacementRegex,
                             perl = TRUE)
              if (STRING_PRIOR == STRING) {
                break
              }
            }
            
            return(STRING)
          },
          .init = LINE)
      })
  
  if (verbose) {
    
    # which packet was analyzed
    cat(crayon::red(pkg, paste(rep("-", 100 - nchar(pkg)),
                               collapse = "")), "\n")
    
    verbolize(script_prior = script_prior,
              script = script,
              lineMatches = lineMatches,
              functions = functions,
              functionsInScript = functionsInScript,
              special_functions = special_functions,
              special_matches = special_matches)
  }
  
  
  if (overwrite) {
    writeLines(script, con = file)
    return(NULL)
  } else {
    return(script)
  }
}
