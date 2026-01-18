#' Searches R Scripts for (Potential) Missings
#'
#' @param script script to originize
#' @param file file name of the script
#' @param functions list of function names to check
#' @param pkgs packages to consider
#' @template verbose
#' @template use_markers
#'
#' @return list of information where which package is (potentially) missing
#' @noRd
originize <- function(dat,
                      files,
                      scripts,
                      functions,
                      pkgs = getOption("origin.pkgs", .packages()),
                      verbose = FALSE,
                      use_markers =
                        getOption("origin.use_markers_for_logging")) {

  # data.frame of function-package pairs
  dat_fctns <- Reduce(f = rbind,
                      x = Map(f = function(pkg, fcts) {
                        data.frame(pkg = paste0(pkg, "::"),
                                   fct = fcts,
                                   pkg_nchar = nchar(pkg) + 2L)},
                        pkgs,
                        functions))

  # set an id column to sort the data into its original order after joins
  dat$Id <- seq_len(nrow(dat))

  # find function calls in scripts
  dat_fctns$MERGE_HELPER <- "FUNCTION_CALL"
  dat <- merge.data.frame(x = dat,
                          y = dat_fctns,
                          by.x = c("text", "usage"),
                          by.y = c("fct", "MERGE_HELPER"),
                          all.x = TRUE,
                          sort = FALSE)

  # sort it in the original order
  dat <- dat[order(dat$Id), ]

  add_pkg <- !is.na(dat$pkg_nchar)
  # cases, for which no corresponding package has been found set added string
  # length to 0
  dat$pkg_nchar[!add_pkg] <- 0


  dat_out <- dat
  dat_out$text[add_pkg] <- paste0(dat_out$pkg[add_pkg],
                                  dat_out$text[add_pkg])


  dat_out <- fix_column_values(dat_out)
  files_rel <- unique(dat_out$file)
  rel <- files %in% files_rel
  result <- stats::setNames(
    Map(f = function(f, s) {
             revert_parse_data(dat_out[dat_out$file == f, ],
                               script = s)
           },
        files[rel],
        scripts[rel]
        ),
    files)




  # if no logging is desired, skip all relevant steps
  if (!verbose) {
    return(list(to_write = result,
                logging_data = any(!is.na(dat_out$pkg))))

  } else {
    dat_logging <- dat

    dat_logging$log_type <- ""

    dat_logging[!is.na(dat_logging$pkg), "log_type"] <- "INSERT"

    # lines with function names that are not used as function names
    dat_logging[dat_logging$text %in% dat_fctns$fct &
                  is.na(dat_logging$pkg) &
                  dat_logging$token == "SYMBOL" &
                  !dat_logging$usage %in% c("NAMESPACED_FUNCTION_CALL",
                                            "METHOD_CALL"),
                "log_type"] <- "MISSING"

    # lines with function names that are not used as function names
    dat_logging[dat_logging$text %in% dat_fctns$fct &
                  is.na(dat_logging$pkg) &
                  dat_logging$token == "SPECIAL", "log_type"] <- "SPECIAL"

    logging_data <- make_logging_data(dat_logging,
                                      use_markers = use_markers,
                                      type_fun = "originize")

    return(list(to_write = result,
                logging_data = logging_data))
  }

}
