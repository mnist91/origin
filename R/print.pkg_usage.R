#' Print the summary of check_pkg_usage
#'
#' @param x a pkg_usage_object
#' @param max_display maximum number of unknown functions or conflicts to print
#' @param ... passed to other methods
#'
#' @return x invisibly
#' @exportS3Method 
#' @importFrom cli cat_rule
#' @importFrom cli style_bold
#' @importFrom cli cli_par
#' @importFrom cli cli_end
#' @importFrom cli cat_line
#' @importFrom cli cli_alert_success
#' @importFrom cli cli_alert_warning
#' @importFrom cli cli_alert_danger
#' @importFrom cli console_width
#' @importFrom cli col_green
#' @importFrom cli col_red
#' @importFrom cli col_yellow
#' @importFrom cli symbol
#'
#' @examples
#' \dontrun{
#' result <- check_pkg_usage()
#' print(result)
#' }
print.pkg_usage <- function(x, max_display = 10L, ...) {
  pkgs <- attr(x, "pkgs")

  # retreive and condense information from data.frame --------------------------
  pkg_in_use <- unique(x[!is.na(x$pkg) & !is.na(x$fun), "pkg"])
  used_pkgs <- setdiff(pkg_in_use,
                       c("base",
                         "user_defined_functions"))
  used_pkgs <- intersect(used_pkgs, pkgs)
  unused_packages <- x[is.na(x$fun), "pkg"]
  other_used_pkgs <- intersect(setdiff(pkg_in_use, pkgs),
                               x[!is.na(x) & x$namespaced, "pkg"])
  undefined_functions <- x[is.na(x$pkg), "fun"]

  # helper functions -----------------------------------------------------------
  cli_header <- function(x) {
    cli::cat_rule(cli::style_bold(x),
                  line_col = "cyan",
                  line = 1)
  }

  into_column <- function(x) {
    nx <- length(x)
    # if there are more than 5 unused packages, put them in 2 columns
    if (nx > 5) {
      # assure a vector of even length
      if (nx %% 2 == 1) {
        x <- append(x, "")
      }
      # paste them together
      col1 <- seq_len(length(x) / 2)
      x <- paste0(x[col1], "     ", x[-col1])
    }
    return(x)
  }

  cnt <- cli::cli_par()
  cli::cat_rule(cli::style_bold("Package Usage Report"), col = "cyan", line = 2)

  # Used Packages --------------------------------------------------------------
  n_used_pckgs <- length(used_pkgs)
  cli_header(paste("Used Packages:", n_used_pckgs))
  if (n_used_pckgs > 0) {
    # style them similiar to cli::cli_alert_success
    used_pkgs <- paste0(
      cli::col_green(cli::symbol$tick), " ",
      format(used_pkgs, width = max(nchar(used_pkgs))))

    # 2 columns if more than 5
    used_pkgs <- into_column(used_pkgs)

    # print the packages
    lapply(used_pkgs, FUN = cli::cat_line)
  }
  cli::cat_line()


  # Unused Packages ------------------------------------------------------------
  n_unused_pckgs <- length(unused_packages)
  cli_header(paste("Unused Packages:", n_unused_pckgs))
  if (n_unused_pckgs > 0) {
    # style them similiar to cli::cli_alert_info
    unused_packages <- paste0(
      cli::col_yellow(cli::symbol$info), " ",
      format(unused_packages, width = max(nchar(unused_packages))))

    # 2 columns if more than 5
    unused_packages <- into_column(unused_packages)

    # print the packages
    lapply(unused_packages, FUN = cli::cat_line)
  }
  cli::cat_line()

  # Conflicts ------------------------------------------------------------------
  if (any(x$conflict, na.rm = TRUE)) {
    dat_conflict <- x[x$conflict %in% TRUE, ]
    n_conflicts <- nrow(dat_conflict)

    # show 10 functions in details at maximum
    too_many <- n_conflicts > max_display
    if (too_many) {
      dat_conflict_add <- dat_conflict[(max_display + 1):n_conflicts, ]
      dat_conflict <- dat_conflict[1:max_display, ]
    }
    # make all unknown functions of equal size
    dat_conflict$fun <- format(dat_conflict$fun,
                               width = min(max(nchar(dat_conflict$fun))))
    dat_conflict$pkg <- format(dat_conflict$pkg,
                               width = min(max(nchar(dat_conflict$pkg))))
    cli_header(paste("Possible Namespace Conflicts: ", n_conflicts))
    lapply(X = paste0(dat_conflict$fun, "    ",
                      dat_conflict$pkg, " >> ",
                      dat_conflict$conflict_pkgs),
           FUN = cli::cli_alert_danger)

    if (too_many) {
      add_conf_str <- paste("... and", nrow(dat_conflict_add), "more:")
      cli::cli_alert_danger(
        paste(
          add_conf_str,
          toString(x     = paste(dat_conflict_add$fun, collapse = ", "),
                   width = max(1,
                               cli::console_width() - nchar(add_conf_str) - 3)
          )
        )
      )
    }
    cli::cat_line()
  }


  # Other Used Packages  -------------------------------------------------------
  n_other_pckgs <- length(other_used_pkgs)
  if (n_other_pckgs > 0) {
    cli_header(paste("Specifically (`pkg::fun()`) further used Packages:",
                     n_other_pckgs))
    # style them similiar to cli::cli_alert_info
    other_used_pkgs <- paste0(
      cli::col_green(cli::symbol$info), " ",
      format(other_used_pkgs, width = max(nchar(other_used_pkgs))))

    # 2 columns if more than 5
    other_used_pkgs <- into_column(other_used_pkgs)

    # print the packages
    lapply(other_used_pkgs, FUN = cli::cat_line)
    cli::cat_line()
  }


  # Undefined Functions --------------------------------------------------------
  n_undefined_funs <- length(undefined_functions)
  if (n_undefined_funs > 0) {
    cli_header(paste("Functions with unknown origin:",
                     n_undefined_funs))
    too_many <- n_undefined_funs > max_display
    if (too_many) {
      undefined_functions_add <-
        undefined_functions[(max_display + 1):n_undefined_funs]
      undefined_functions <- undefined_functions[1:max_display]
    }

    # style them similiar to cli::cli_alert_danger
    undefined_functions <- paste0(
      cli::col_red(cli::symbol$cross), " ",
      format(undefined_functions, width = max(nchar(undefined_functions))))

    # 2 columns if more than 5
    undefined_functions <- into_column(undefined_functions)

    # print the functions
    lapply(undefined_functions, FUN = cli::cat_line)

    if (too_many) {
      add_undef_str <-
        paste("... and", length(undefined_functions_add), "more:")
      cli::cli_alert_danger(
        paste(add_undef_str,
              toString(x     = paste(undefined_functions_add, collapse = ", "),
                       width = max(1,
                                   cli::console_width() -
                                     nchar(add_undef_str) - 3)
              )))
    }

  } else {
    cli::cli_alert_success("All used functions defined! \U0001F973")
  }

  cli::cli_end(cnt)

  return(invisible(x))
}
