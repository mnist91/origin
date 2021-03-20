#' Get All Exported Functions From a Package
#'
#' @param pkg string of a package name
#'
#' @return character vector of functions names
#' @export
#'
#' @examples
#' get_exported_functions("data.table")
#'
get_exported_functions <- function(pkg) {
  # get all exported functions from a package --------------------------------
  # lists all exports of a package (incl. non functions)
  exports <- getNamespaceExports(pkg)
  
  is_function <- vapply(exports, 
                        FUN = function(x, ns) {
                          inherits(x = getExportedValue(ns, x),
                                   what = "function")
                        },
                        FUN.VALUE = logical(1),
                        ns = getNamespace(pkg))
  
  # overlap of exports and functions -> exported functions
  functions <- exports[is_function]
  
  return(functions)
}


#' Which functions are potentially used in the script
#' 
#' This is a fast check to extract potentialy used functions. It only checks if
#' the function name strings are present in any way. This reduces the number
#' of functions that must be considered more closely significantly. It speeds
#' up all further steps
#'
#' @param script a script to check
#' @param functions a vector with function names
#' @param ignore_comments a boolean, if TRUE lines starting with # are ignored
#' @param pkg package name from which the functions stem from
#' @template verbose
#'
#' @return
#' @noRd
#'
check_functions <- function(script,
                           functions,
                           ignore_comments = TRUE,
                           pkg = NULL,
                           verbose = TRUE) {
  # remove function with special characters like %, &, [] and :
  special_functions <- grepl(functions, pattern = "\\&|\\:|\\]|\\[|%")
  relevant_functions <- functions[!special_functions]
  # reduce the number of functions to check, by selecting possible (occurring)
  # functions. This isn't a check if it is a function or an object,
  # but a simple regular expression
  
  full_script <- paste0(script, collapse = "")
  
  matches <- vapply(X = relevant_functions,
                    FUN = function(fun) {
                      grepl(pattern = fun,
                            x = full_script,
                            fixed = TRUE)
                    },
                    FUN.VALUE = logical(1))
  functions_in_script <- relevant_functions[matches]
  
  # special functions such as %like" can not be called with ::
  # print a warning, that such functions occur
  special_matches <- vapply(X = functions[special_functions],
                            FUN = function(fun) {
                              grepl(pattern = fun,
                                    x = full_script,
                                    fixed = TRUE)
                            },
                            FUN.VALUE = logical(1))
  
  # no matching functions
  if (!any(matches)) {
    
    if (any(special_matches)) {
      special_functions_in_script <-
        functions[special_functions][special_matches]
      
      special_matches <- which(
        as.logical(
          Reduce(f = "+",
                 x = lapply(X = special_functions_in_script,
                            FUN = function(pattern) {
                              grepl(x = script,
                                    pattern = pattern,
                                    fixed = TRUE)
                            }))
        ))
    }
    return(NULL)
  }
  
  # reduce the number of script rows to check, by selecting only those which
  # contain a function name
  line_matches <- grepl(x = script,
                        pattern = paste(functions_in_script, collapse = "|"))
  
  # ignore comment rows
  if (ignore_comments) {
    line_comments <- grepl(x = trimws(script[line_matches]), pattern = "^#")
    
    # ignore these line for the matching
    line_comments_matches <- which(line_matches)[which(line_comments)]
    line_matches[line_comments_matches] <- FALSE
    
  }
  
  
  return(list(matches = matches,
              line_matches = line_matches,
              special_matches = special_matches,
              special_functions = special_functions,
              functions_in_script = functions_in_script))
  
}


#' Unlist a list into a vector with names equal to fromer list element name
#'
#' @param l a list to convert into a vector
#' @param nms character vector of same length as list, defaults to the 
#'   names of l
#'
#' @details the `base::unlist` function converts a list into a vector yet
#'    assigns unique names to each vector element, More precisely, it adds
#'     a number to the name of its list element. This function does not
#'     create unique names but assigns the bare name of the list element
#'     to all vector elements that stem from this list element
#'   
#' @return named vector
#' @export
#'
#' @examples
#' l <- list(rot = 1:3, blau = 1:2)
#' un_list(l)
#' # >  rot  rot  rot blau blau
#' # >    1    2    3    1    2
#' @importFrom stats setNames

un_list <- function(l, nms = names(l)) {
  out <- unlist(
    recursive = TRUE,
    use.names = TRUE,
    x = mapply(
      FUN = function(x, nm) {
        setNames(x, rep(nm, length(x)))
      },
      l,
      nms,
      SIMPLIFY = FALSE,
      USE.NAMES = FALSE
    )
  )
  return(out)
}

# style a string for logging output
# TODO: colors depending on background /theme
add_logging <- function(string, splits, pkg, log_length, type, use_markers) {
  if (use_markers) {
    ins_start_string <- sprintf('<text style="color: %s;">',
                                getOption("origin.color_added_package"))
    mis_start_string <-  sprintf('<text style="color: %s;">',
                                 getOption("origin.color_missed_function"))
    spe_start_string <-  sprintf('<text style="color: %s;">',
                                 getOption("origin.color_special_function"))
    end_string <- "</text>"
    start_wrapper <- "<div>"
    end_wrapper <- "</div"
  } else {
    # TODO: color codes depending on theme
    ins_start_string <- "\033[36m"
    mis_start_string <- "\033[31m"
    spe_start_string <- "\033[33m"
    end_string <- "\033[39m"
    start_wrapper <- "\033[39m"
    end_wrapper <- "\033[39m"
  }
  
  splitted <- substring(text = string,
                        first = c(1, splits),
                        last = c(splits - 1, nchar(string)))
  
  to_highlight <- log_length != 0
  
  splitted[-1][to_highlight] <- as.character(
    Map(function(str, len, type) {
      to_log <- substring(text = str,
                          first = c(1, len + 1),
                          last = c(len, nchar(str)))
      if (type == "missed") {
        start_string <- mis_start_string
      } else {
        start_string <- spe_start_string
      }
      paste(start_string, to_log[1], end_string, to_log[2], sep = "")
      
    },
    splitted[-1][to_highlight],
    log_length[to_highlight],
    type[to_highlight])
  )
  to_insert <- log_length == 0
  splitted[-1][to_insert] <- paste(ins_start_string, 
                                   pkg[to_insert], end_string,
                                   splitted[-1][to_insert],
                                   sep = "")
  
  out <- paste0(start_wrapper,
                splitted[1],
                paste(splitted[-1],
                      collapse = ""),
                end_wrapper)
  
  return(out)
}

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
  
  data.frame(line = line,
             string = string_after,
             stringsAsFactors = FALSE)
  
}

# insert package:: at positions
add_package <- function(string, splits, pkg) {
  splitted <- substring(text = string,
                        first = c(1, splits),
                        last = c(splits - 1, nchar(string)))
  out <- paste0(splitted[1], paste0(pkg, splitted[-1], collapse = ""))
  return(out)
}


# keep results of greprex where a match has been found
comb_matches <- function(x, y) {
  x <- x[x != -1]
  y <- y[y != -1]
  c(x, y)
}


# assign new script
apply_changes <- function(ask_before_applying_changes, result) {
  if (ask_before_applying_changes && interactive()) {
    cat("\nHappy with the result? \U0001f600\n\n")
    answer <- menu(choices = c("YES", "NO"))
    if (answer != 1) {
      message("No changes made!")
      return(invisible(NULL))
    }
  } 
  
  lapply(X = Filter(result, f = function(l) !is.null(l$script)), 
         FUN = function(x) writeLines(text = x$script, con = x$file))
  
  return(invisible(NULL))
}


# named character vector of functions with package name as names
get_fun_duplicates <- function(functions) {
  funs_unlisted <- un_list(functions, nms = names(functions))
  
  funs_duplicates <- funs_unlisted[which(duplicated(funs_unlisted))]
  dups <- funs_unlisted[funs_unlisted %in% funs_duplicates]
  
  return(dups)
}



# exclude functions from originizing
exclude_functions <- function(funs, to_exclude) {
  # functions to be exlcluded from specific packages
  has_name <- nzchar(names(to_exclude))
  
  # functions to exclude regardless of package
  if (all(!has_name)) {
    out <- lapply(X = funs, FUN = setdiff, y = to_exclude)
    
    # functions to exclude from specific packages
  } else if (all(has_name)) {
    out <- mapply(
      FUN = setdiff_specific,
      funs = funs,
      pkg  = names(funs),
      MoreArgs  = list(excl = to_exclude),
      SIMPLIFY  = FALSE,
      USE.NAMES = TRUE)
    
    # some functions from specific packages, some unspecified
  } else {
    out <- lapply(X = funs, FUN = setdiff, y = to_exclude[!has_name])
    out <- mapply(
      FUN = setdiff_specific,
      funs = out,
      pkg  = names(out),
      MoreArgs  = list(excl = to_exclude[has_name]),
      SIMPLIFY  = FALSE,
      USE.NAMES = TRUE)
  }
  
  return(out)
}

# exclude functions from specific packages
setdiff_specific <- function(funs, pkg, excl) {
  # functions to exclude from the current package
  to_exclude <- unlist(excl[names(excl) == pkg])
  
  # excludable functions in the current package
  matches <- funs %in% to_exclude
  
  # functions that are not in the specified package
  if (sum(matches) != length(to_exclude)) {
    funs_not_found <- to_exclude[!to_exclude %in% funs]
    warning(sprintf("Excludable function %s is not exported by package %s\n",
                    funs_not_found,
                    pkg))
  }
  return(funs[!matches])
}
