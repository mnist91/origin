# Insert all package:: to a line
prep_line_originize <- function(line, lines, matches, pkg, string) {
  rel <- lines == line

  matches <- un_list(matches[rel], pkg[rel])

  # account for functions that are exported by multiple packages
  # first evaluated function wins
  replace_matches <- sort(matches[!duplicated(matches)])
  string_after <- add_package(string = string[rel][1],
                              splits = replace_matches,
                              pkg = names(replace_matches))

  return(
    data.frame(line = line,
               string = string_after,
               stringsAsFactors = FALSE)
  )
}

# insert package:: at positions
add_package <- function(string, splits, pkg) {
  splitted <- substring(text = string,
                        first = c(1, splits),
                        last = c(splits - 1, nchar(string)))
  out <- paste0(splitted[1], paste0(pkg, splitted[-1], collapse = ""))
  return(out)
}

