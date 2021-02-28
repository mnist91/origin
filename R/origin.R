#' Add Package Name to Function Calls
#'
#' @param pkg
#' @param file
#' @param overwrite
#' @param ignoreComments
#' @param verbose
#' @param functions
#'
#' @return
#' @export
#'
#' @examples
addPackageToFunction <- function(pkg,
                                 file = NULL,
                                 overwrite = FALSE,
                                 ignoreComments = TRUE,
                                 verbose = FALSE,
                                 functions = NULL) {


  if(!file.exists(file)) {
    stop("No file in this path\n", file)
    # Lese file ein
  } else {
    script <- readLines(file)
  }

  # Eingangsstatus für späteren Vergleich
  if(verbose){
    script_prior <- script
  }

  if(is.null(functions)) {
    # lese alle exportierten Funktionen aus dem Paket ein ---------------------------
    # listet alle, auch nicht exportierten Funktionen des Pakets, auf
    functions <- lsf.str(paste0("package::", pkg),
                         envir = getNamespace(pkg))
    # listet alle Exports des Pakets auf, auch nicht-FUnktionen
    exports <- getNamespaceExports(pkg)
    # Schnittmenge aus Fnúnktionen und exports
    functions <- functions[functions %in% exports]
  }

  l <- checkFunctions(script = script,
                      functions = functions,
                      pkg = pkg,
                      verbose = verbose)

  # make function-Output available
  matches = l$matches
  lineMatches = l$lineMatches
  special_matches = l$special_matches
  functionsInScript = l$functionsInScript
  special_functions = l$special_functions
  if(!any(matches)) {
    return(NULL)
  }


  # regex, um Paketname Funktionsname voranzustellen
  replacementRegex <- paste0("\\1", pkg, "::\\2\\3")

  # regex, um funcktional genutzte Funktionen in *apply/purrr zu erkennen
  funArguments <- c("FUN", "\\.f")
  assignWithSpaces <- c("=", " = ", "= ", " =")
  leadingPatterns <- paste(apply(expand.grid(funArguments, assignWithSpaces),
                                 MARGIN = 1,
                                 FUN = paste0,
                                 collapse = ""),
                           collapse = "|")

  script[lineMatches] <-
    # iteriere über alle Zeilen des Skripts
    purrr::map_chr(
      .x = script[lineMatches],
      .f = function(LINE) {
        # iteriere über alle Funktionen.
        # nutze reduce um jeweils vorherige ersetzte funktion nicht zu überschreiben
        purrr::reduce(
          .x = functionsInScript,
          .f = function(STRING, FUN) {

            # wird die selbe Funktion mehrfach in einer Zeile aufgerufen
            # wird dies von gsub nicht mehrmals ersetzt
            # daher while-loop.
            # potentiell umwandeln in while loop oder gsub zum laufen bringen

            # Funktionsname muss nach einem Komma, Leerzeichen oder einer
            # offenen Klammer kommen ODER am anfang der Zeile stehen
            # Außerdem gefolgt von einer offnen runden Klammer
            FUN <- gsub("\\.", "\\\\.", x = FUN)
            patternRegex <- paste0("(.*)((?<=[, \\(]|^)", FUN, " *\\()(.*)")

            # beim funktionalen programmieren wird die Funktion ohne Klammern
            # sondern als Objekt aufgerufen. Eine unterscheidung zwischen
            # einer Funktion und einer sonstigen Avriable ist hier nur dadurch
            # möglich, dass das Funktionsargument explizit via "FUN = "
            # (apply-Funktionen) oder ".f = " gesetzt wird,
            functionalPatternRegex <- paste0("(.*)((?<=", leadingPatterns, ")", FUN, "(?=[ \\,)]|$))(.*)")

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
              if(STRING_PRIOR == STRING) {
                break
              }
            }

            return(STRING)
          },
          .init = LINE)
      })

  if(verbose) {

    # welches Paket wurde betrachtet
    cat(crayon::red(pkg, paste(rep("-", 100 - nchar(pkg)), collapse = "")), "\n")

    verbolize(script_prior = script_prior,
              script = script,
              lineMatches = lineMatches,
              functions = functions,
              functionsInScript = functionsInScript,
              special_functions = special_functions,
              special_matches = special_matches)
  }


  if(overwrite){
    writeLines(script, con = file)
    return(NULL)
  } else {
    return(script)
  }
}

