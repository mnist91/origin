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
