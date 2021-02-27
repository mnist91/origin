#' Get All Exported Functions From a Package
#'
#' @param pkg `character(1)`
#'   Name of package
#'
#' @return `character(1)`
#' @export
#'
#' @examples
#' getFunctions("data.table)
getFunctions <- function(pkg) {
  # lese alle exportierten Funktionen aus dem Paket ein ---------------------------
  # listet alle, auch nicht exportierten Funktionen des Pakets, auf
  functions <- lsf.str(paste0("package::", pkg),
                       envir = getNamespace(pkg))
  # listet alle Exports des Pakets auf, auch nicht-FUnktionen
  exports <- getNamespaceExports(pkg)
  # Schnittmenge aus Fnúnktionen und exports
  functions <- functions[functions %in% exports]

  return(functions)
}


#' Extract relevant Function informations
#'
#' @param script
#' @param functions
#' @param ignoreComments
#'
#' @return
#' @export
#'
#' @examples
checkFunctions <- function(script, functions, ignoreComments = TRUE, pkg = NULL, verbose = TRUE) {
  # entferne bestimmte Funktionen mit special characters
  special_functions <- grepl(functions, pattern = paste("\\$", "\\:", "\\]", "\\[", "%", sep = "|"))
  relevant_functions <- functions[!special_functions]
  # Um Anzahl der zu überprüfenden Funktionen zu reduzieren, überprüfe, welche
  # Funktionsnamen überhaupt im file potentiell genutzt werden.
  # Hier findet noch keine Überprüfung statt, ob es sich hier lediglich um
  # andere Objekte handelt
  fullScript <- paste0(script, collapse = "")

  # TODO: performance of sschneller mit
  # any(grepl(pattern = .x, x = script, fixed = TRUE))
  matches <- purrr::map_lgl(relevant_functions,
                            ~grepl(pattern = .x, x = fullScript, fixed = TRUE))
  functionsInScript <- relevant_functions[matches]

  # Spezialfunktionen wie %like% können nicht mit :: aufgerufen werden.
  # Printe Warnung, dass diese genutzt werden
  special_matches <- purrr::map_lgl(functions[special_functions],
                                    ~grepl(pattern = .x, x = fullScript, fixed = TRUE))

  # Keine Funktionen zu ersetzen
  if(!any(matches)) {
    if(!verbose) {
      return(NULL)
    }
    if(!is.null(pkg)) {
      cat(crayon::red(pkg, paste(rep("-", 100 - nchar(pkg)), collapse = "")), "\n")
    }
    cat(crayon::green(0, "Lines changed\n"))
    cat(crayon::green(0, "Functions recognized\n"))

    if(any(special_matches)) {
      special_functions_in_script <- functions[special_functions][special_matches]
      cat(crayon::magenta("Special Functions used!"), "\n")
      specialMatches <- which(as.logical(Reduce(f = "+", purrr::map(.x = special_functions_in_script,
                                                                    .f = ~grepl(x = script, pattern = .x, fixed = TRUE)))))
      cat(paste(paste("Line ", specialMatches, ": ", script[specialMatches], sep = ""),
                collapse = "\n"))
    }
    cat("\n")
    return(NULL)
  }

  # Reduziere die Anzahl der zu überprüfenden Zeilen, welche überhaupt
  # funktionsnamen enthalten
  lineMatches <- grepl(x = script, pattern = paste(functionsInScript, collapse = "|"))

  # Kommentarzeilen ignorieren
  if(ignoreComments) {
    # beginnt mit # oder es kommen nur Öeerzeichen vor dem #
    # startsWithHashRegEx <- "(?<=^[ *]|^)#"
    lineComments <- grepl(x = gsub(x = script[lineMatches], pattern = " ", replacement = ""),
                          pattern = "^#")

    # beachte diese Zeilen nicht beim ersetzen
    lineCommentsMatches <- which(lineMatches)[which(lineComments)]
    lineMatches[lineCommentsMatches] <- FALSE

  }


  return(list(matches = matches,
              lineMatches = lineMatches,
              special_matches = special_matches,
              special_functions = special_functions,
              functionsInScript = functionsInScript))

}
