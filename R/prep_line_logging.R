#' Set color highlighting for each line
#'
#' @param line Current line to style.
#' @param lines integer vector of line information.
#' @param matches logical vector
#' @param pkg character of current package
#' @param log_length boolean whether to use log_length
#' @param type character which type is present
#' @template use_markers
#'
#' @return data.frame
#' @noRd
prep_line_logging <- function(line,
                              lines,
                              matches,
                              pkg,
                              log_length,
                              type,
                              string,
                              use_markers) {
  # Each line can bear multiple insertions /
  # missings / infixes. Since each of these types come in a separate element,
  # all elements connected to the line have to be extracted
  rel <- lines == line

  # named vector of positions to insert or highlight text. starting position
  matches <- un_list(matches[rel], pkg[rel])

  # how many characters to highlight starting from the matches position
  match_length <- unlist(log_length[rel])

  # kind of highlighting. either insert, missed or infix
  match_type <- rep(type[rel],
                    lapply(X = log_length[rel],
                           FUN = length))

  # insertions and missings are determined separately. Therefore, each insertion
  # is regularly a missing as well. In this step the insertion beats the missing
  # since it has been evaluated priorly.
  # Given function setDT. It has been found by insertion as well as by missing
  # since the find-missings-algorithm was not aware of insertions.
  dups <- !duplicated(matches)
  replace_matches <- matches[dups]
  replace_lengths <- match_length[dups]
  replace_types <- match_type[dups]

  # order highlighting by column position in line
  ord <- order(replace_matches)
  replace_matches <- replace_matches[ord]
  replace_lengths <- replace_lengths[ord]
  replace_types <- replace_types[ord]

  # add the colour highlighting
  string_after <- add_logging(string = string[rel][1],
                              splits = replace_matches,
                              pkg = names(replace_matches),
                              log_length = replace_lengths,
                              type = replace_types,
                              use_markers = use_markers)

  # return information for usage in markers
  # TODO: check which are necessary outside of use_markers
  data.frame(line = line,
             message = string_after,
             type = set_marker_type(replace_types, use_markers),
             column = min(replace_matches),
             stringsAsFactors = FALSE)

}

