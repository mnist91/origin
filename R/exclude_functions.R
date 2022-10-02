# exclude functions from originizing
exclude_functions <- function(funs, to_exclude) {
  # functions to be exlcluded from specific packages
  has_name <- nzchar(names(to_exclude))

  # functions to exclude regardless of package
  if (all(!has_name) ) {
    out <- lapply(X = funs, FUN = setdiff, y = unlist(to_exclude))

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
