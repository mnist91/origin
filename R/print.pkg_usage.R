#' Print the summary of check_pkg_usage
#'
#' @param x a pkg_usage_object
#' @param ... passed to other methods
#'
#' @return x invisibly
#' @exportS3Method 
#'
#' @examples
#' \dontrun{
#' result <- check_pkg_usage()
#' print(result)
#' }
print.pkg_usage <- function(x, ...) {
  pkgs <- attr(x, "pkgs")
  
  pkg_in_use <- unique(x[!is.na(x$pkg) & !is.na(x$fun), "pkg"])
  used_pkgs <- setdiff(pkg_in_use,
                       c("stats", "graphics", "grDevices",
                         "datasets", "utils", "methods", "base",
                         "user_defined_functions"))
  used_pkgs <- intersect(used_pkgs, pkgs)
  unused_packages <- x[is.na(x$fun), "pkg"]
  other_used_pkgs <- setdiff(pkg_in_use, pkgs)
  undefined_functions <- x[is.na(x$pkg), "fun"]
  
  
  cat("Used Packages:", length(used_pkgs), "\n\n")
  if (length(used_pkgs) > 0) {
    cat("\t", paste(used_pkgs, collapse = ", "), "\n\n", sep = "")
  }
  
  cat("Unused Packages:", length(unused_packages), "\n\n")
  if (length(unused_packages) > 0) {
    cat("\t", paste(unused_packages, collapse = ", "), "\n\n", sep = "")
  }
  
  if (any(x$conflict, na.rm = TRUE)) {
    dat_conflict <- x[x$conflict %in% TRUE, ]
    max_display <- 10
    # show 10 functions in details at maximum
    more_than_10 <- nrow(dat_conflict) > max_display
    if (more_than_10) {
      dat_conflict_add <- dat_conflict[(max_display + 1):nrow(dat_conflict), ]
      dat_conflict <- dat_conflict[1:max_display, ]
    }
    # make all unknown functions of equal size
    dat_conflict$fun <- format(dat_conflict$fun, 
                               width = min(max(nchar(dat_conflict$fun))))
    cat("Possible Namespace Conflicts: ", nrow(dat_conflict), "\n\n")
    cat(paste("\t", dat_conflict$fun, "\t",
              dat_conflict$pkg, " >> ", dat_conflict$conflict_pkgs,
              sep = "", collapse = "\n"))
    if (more_than_10) {
      cat("\n\tand", nrow(dat_conflict_add), "more:", 
          toString(dat_conflict_add$fun))
    }
    cat("\n\n")
  }
  
  
  cat("Specifically (`pkg::fun()`) further used Packages:",
      length(other_used_pkgs),
      "\n\n")
  if (length(other_used_pkgs) > 0) {
    cat("\t", paste(other_used_pkgs, collapse = ", "), "\n\n", sep = "")
  }
  
  if (length(undefined_functions) > 0) {
    cat("Functions with unknown origin:", length(undefined_functions), "\n\n")
    cat("\t", paste(undefined_functions, collapse = ", "), "\n\n", sep = "")
    
    
  } else {
    cat("All used functions defined! \U0001F973 \n")
  }
  
  return(invisible(x))
}

