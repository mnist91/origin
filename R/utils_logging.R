#' Print Change Log and potential Errors in Concole
#'
#' @param script_prior
#' @param script_after
#' @param lineMatches
#' @param functions
#' @param functionsInScript
#' @param special_functions
#' @param special_matches
#'
#' @return
#' @export
#'
#' @examples
verbolize <- function(script_prior,
                      script_after,
                      lineMatches = rep(TRUE, length(script_after)),
                      functions,
                      functionsInScript,
                      special_functions = NULL,
                      special_matches = FALSE) {
  # zeilen, die verändert wurden
  changes <- script_after[lineMatches] != script_prior[lineMatches]
  line_matches_pos <- which(lineMatches)

  # Anzahl geänderter Zeilen
  sumChanges <- sum(changes)
  cat(crayon::green(sumChanges, "Lines changed\n"))

  # Auflistung erkannter Funktionen aus dem Paket im Skript
  cat(crayon::green(length(functionsInScript), "Functions recognized\n"))
  cat(paste(functionsInScript, collapse = "\n"), "\n")

  # Auflistung aller veränderter Zeilen
  cat(crayon::green("Changes:\n"))

  changedStrings <- purrr::map2_chr(
    .x = script_prior[lineMatches][changes],
    .y = script_after[lineMatches][changes],
    .f = function(prior, after) {
      # füge leeren string ein, wenn es einen unterschied zwischen string 1 und string 2 gibt
      prior <- strsplit(prior, split = "")[[1]]
      after <- strsplit(after, split = "")[[1]]

      for(i in seq_along(after)) {
        if(prior[i] != after[i]) {
          prior[i:(length(prior) + 1)] <- c("", prior[i:length(prior)])
        }
      }

      # Färbe Buchstaben ein, die bei afetr hinzugekommen sind
      # Siehe crayon::green("TEST)
      diffs <- !nzchar(prior)
      after[diffs] <- paste0("\033[36m", after[diffs], "\033[39m")

      after <- paste(after, collapse = "")
      return(after)
    })
  cat(paste(paste("Line ", line_matches_pos[changes], ": ", changedStrings, sep = ""),
            collapse = "\n"), "\n")


  # Zeilen, in denen Funktionsnamen vorkommen, die sich aber nicht verändert haben
  # NICHT allumfassend, da in einer Zeile eine Funktion erkannt worden sein könnte, die andere nicht
  potential_missings <- script_after[lineMatches]#[!changes]
  # färbe verpasste Funktionen rot ein.
  # Siehe crayon::yellow("TEST)
  replacementRegex <- paste0("\\1\033[33m\\2\033[39m\\3")
  potential_missings_unchanged <-
    # iteriere über alle Zeilen des Skripts
    purrr::map_chr(
      .x = potential_missings,
      .f = function(LINE) {
        # iteriere über alle Funktionen.
        # nutze reduce um jeweils vorherige ersetzte funktion nicht zu überschreiben
        purrr::reduce(.x = functionsInScript,
                      .f = function(STRING, FUN) {

                        # wird die selbe Funktion mehrfach in einer Zeile aufgerufen
                        # wird dies von gsub nicht mehrmals ersetzt
                        # daher repeat-loop.
                        # potentiell umwandeln in while loop oder gsub zum laufen bringen

                        # Funktionsname , der nicht nach "::" kommt oder nach der
                        # Farbsequenz
                        FUN <- gsub("\\.", "\\\\.", x = FUN)
                        patternRegex <- paste0("(.*)((?<!::|33m)", FUN, ")(.*)")
                        repeat{
                          STRING_PRIOR <- STRING
                          STRING <- gsub(x = STRING,
                                         replacement = replacementRegex,
                                         pattern = patternRegex,
                                         perl = TRUE)
                          if(STRING_PRIOR == STRING) {
                            break
                          }
                        }

                        return(STRING)
                      },
                      .init = LINE)
      })

  potential_missings_unchanged <- potential_missings_unchanged[potential_missings_unchanged != potential_missings]
  if(length(potential_missings_unchanged) > 0){
    cat("\n",crayon::blue("Function names are not used like functions. Check for variable names or ",
                          "functional programming in *apply/purrr"), "\n")
    cat(paste(paste("Line ", line_matches_pos, #[!changes],
                    ": ", potential_missings_unchanged, sep = ""),
              collapse = "\n"), "\n")
  }

  # wurden Spezialfunktionen wie %like% oder %>% genutzt, die nicht mit
  # PACKAGE:: vorangestellt genutzt werden können?
  if(any(special_matches)) {
    cat("\n", crayon::magenta("Special functions used!"), "\n")

    special_functions_in_script <- functions[special_functions][special_matches]
    # Welche Zeilen enthielten mindestens eine solche Spezialfunktion
    specialMatches <- which(as.logical(Reduce(f = "+",
                                              purrr::map(.x = special_functions_in_script,
                                                         .f = ~ grepl(x = script_after,
                                                                      pattern = .x,
                                                                      fixed = TRUE)))))
    # Welche Spezialfunktion wurde in welcher Zeile genutzt?
    # Durch die Special-Characters in diesen Funktionen ist eine regEx-Suche nicht möglich
    # fixed muss auf TRUE gesetzt sein und über die Funktionen iteriert werden
    # Jedes Listenelement steht für eine special-Funktion und die Zahlen darin stehen
    # für die Zeile, in der diese genutzt wird
    specialsFound <- purrr::map(.x = special_functions_in_script,
                                .f = ~grep(x =  script_after[specialMatches], pattern = .x, fixed = TRUE))

    # entsprechender Funktionsname als name
    names(specialsFound) <- special_functions_in_script

    # liste als ein Vektor, wobei der name jeweils der Funktionsname ist
    specialsUnlisted <- unlist(purrr::transpose(specialsFound))
    # Aggregiere alle Funktionen nach Zeilen zusammen
    funsInLine <- by(names(specialsUnlisted), specialsUnlisted, paste, collapse = ", ")

    # Für alignment, mache alle Funktions-Strings gleich lang
    funsInLine <- format(funsInLine, width = max(nchar(funsInLine)))

    # Output highlighting wegen special Characters nicht möglich
    cat(paste(paste("Line ", specialMatches, ": ",
                    funsInLine, "\t",
                    script_after[specialMatches],
                    sep = ""),
              collapse = "\n"), "\n")
  }
  cat("\n\n")

}

