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
  # lists all exports of a package (incl. non functions)
  exports <- getNamespaceExports(pkg)
  
  is_function <- vapply(exports, 
                        FUN = function(x, ns) {
                          inherits(x = getExportedValue(ns, x), what = "function")
                        },
                        FUN.VALUE = logical(1),
                        ns = getNamespace(pkg))
  
  # overlap of exports and functions -> exported functions
  functions <- exports[is_function]
  
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
  
  matches <- vapply(X = relevant_functions,
                    FUN = function(fun) {
                      grepl(pattern = fun,
                            x = fullScript,
                            fixed = TRUE)
                    },
                    FUN.VALUE = logical(1))
  functions_in_script <- relevant_functions[matches]
  
  # special functions such as %like" can not be called with ::
  # print a warning, that such functions occur
  special_matches <- vapply(X = functions[special_functions],
                            FUN = function(fun) {
                              grepl(pattern = fun,
                                    x = fullScript,
                                    fixed = TRUE)
                            },
                            FUN.VALUE = logical(1))
  
  # no matching functions
  if (!any(matches)) {
    
    if (any(special_matches)) {
      special_functions_in_script <-
        functions[special_functions][special_matches]
      
      specialMatches <- which(
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
  if (ignoreComments) {
    # starts with # or leading spaces and #
    # startsWithHashRegEx <- "(?<=^[ *]|^)#"
    lineComments <- grepl(x = trimws(script[line_matches]), pattern = "^#")
    
    # ignore these line for the matching
    lineCommentsMatches <- which(line_matches)[which(lineComments)]
    line_matches[lineCommentsMatches] <- FALSE
    
  }
  
  
  return(list(matches = matches,
              line_matches = line_matches,
              special_matches = special_matches,
              special_functions = special_functions,
              functions_in_script = functions_in_script))
  
}


# named list to a named vector with names corresponding
# to prior name of its list element
# l <- list(rot = 1:3, blau = 1:2)
# get_named_vec(l)
# # >  rot  rot  rot blau blau
# # >    1    2    3    1    2
get_named_vec <- function(LIST, nms = names(LIST)) {
  out <- unlist(
    unname(
      Map(function(x, nm) {
        setNames(x, rep(nm, length(x)))
      },
      LIST,
      nms
      )
    )
  )
  return(out)
}

# style a string for logging output
# TODO: color codes as arguments
# TODO: colors depending on background /theme
add_logging <- function(string, splits, pkg, log_length, type, use_markers = TRUE) {
  if(use_markers) {
    ins_start_string <- '<text style="color: red;">'
    ins_end_string <- '</text>'
    mis_start_string <- '<text style="color: yellow;">'
    mis_end_string <- '</text>'
    spe_start_string <- '<text style="color: green;">'
    spe_end_string <- '</text>'
  } else {
    # TODO: color codes abhängig von Theme
    miss_start_string <- '33m['
    miss_end_string <- 'XXXX'
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
      if(type == "missed") {
        start_string <- mis_start_string
        end_string <- mis_end_string
      } else {
        start_string <- spe_start_string
        end_string <- spe_end_string
      }
      paste(start_string, to_log[1], end_string, to_log[2], sep = "")
      
    },
    splitted[-1][to_highlight],
    log_length[to_highlight],
    type[to_highlight])
  )
  to_insert <- log_length == 0
  splitted[-1][to_insert] <- paste(ins_start_string, 
                                   pkg[to_insert], ins_end_string,
                                   splitted[-1][to_insert],
                                   sep = "")
  
  out <- paste0(splitted[1],
                paste(splitted[-1],
                      collapse = ""))
  
  return(out)
}

# Insert all package:: to a line
prep_line_originize <- function(line, lines, matches, pkg, string) {
  rel <- lines == line
  
  matches <- get_named_vec(matches[rel], pkg[rel])
  
  # account for functions that are exported by multiple packages
  # first evaluated function wins
  replace_matches <- sort(matches[!duplicated(matches)])
  string_after <- add_package(string = string[rel][1],
                              splits = replace_matches,
                              pkg = names(replace_matches))
  
  data.frame(line = line,
             string = string_after)
  
}

# insert package:: at positions
add_package <- function(string, splits, pkg) {
  splitted <- substring(text = string,
                        first = c(1, splits),
                        last = c(splits - 1, nchar(string)))
  out <- paste0(splitted[1], paste0(pkg, splitted[-1], collapse = ""))
  return(out)
}

# combined color highlighting for each line 
prep_line_logging <- function(line, logging_comb, use_markers) {
  rel <- logging_comb$line == line
  
  matches <- get_named_vec(logging_comb$matches[rel], logging_comb$pkg[rel])
  match_length <- unlist(logging_comb$log_length[rel])
  match_type <- rep(logging_comb$type[rel], 
                    lapply(X = logging_comb$log_length[rel],
                           FUN = length))
  dups <- !duplicated(matches)
  replace_matches <- matches[dups]
  replace_lengths <- match_length[dups]
  replace_types <- match_type[dups]
  ord <- order(replace_matches)
  
  replace_matches <- replace_matches[ord]
  replace_lengths <- replace_lengths[ord]
  replace_types <- replace_types[ord]
  
  string_after <- add_logging(string = logging_comb$string[rel][1],
                              splits = replace_matches,
                              pkg = names(replace_matches),
                              log_length = replace_lengths,
                              type = replace_types,
                              use_markers = use_markers)
  
  data.frame(line = line,
             message = string_after)
  
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
    cat("Happy with the result? \U0001f600\n\n")
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
  funs_unlisted <- get_named_vec(functions, nms = names(functions))
  
  funs_duplicates <- funs_unlisted[which(duplicated(funs_unlisted))]
  dups <- funs_unlisted[funs_unlisted %in% funs_duplicates]
  
  return(dups)
}


# print warning regarding conflicts
solve_fun_duplicates <- function(dups, pkgs) {
  # Require User interaction if duplicates are detected
  crayon_danger <- crayon::combine_styles(crayon::red,
                                          crayon::underline,
                                          crayon::bold)
  cat(crayon_danger("Used functions in mutliple Packages!"), "\n")
  dups_with_package <- by(names(dups), dups, paste, collapse = ", ")
  
  cat(paste(dups_with_package, ": ", names(dups_with_package),
            collapse = "\n", sep = ""),
      "\n")
  cat("Order in which relevant packges are evaluated;\n\n")
  cat(paste(pkgs[pkgs %in% names(dups)], collapse = " >> "), "\n")
  
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
