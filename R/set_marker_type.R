#' Determine (marker) type for each line
#'
#' The Markers Tab in RStudio can handle different types which are indicated
#' by an icon in front of the line. Which kind of info (insert, missed, infix)
#' belongs to which marker type is specified here. If different kinds are
#' present in the same line, an order is specified which is the most important
#' type. The same logic is applied to a console logging output
#'
#' @param x vector of highlighting types of the subset insert, infix, missed
#' @template use_markers
#'
#' @noRd
#' @return string
set_marker_type <- function(x,
                            use_markers = TRUE) {

  # all available markers type options in order of importance
  # each marker entry has assigned one type only
  if (use_markers) {
    mapping <- list(SPECIAL = "box",
                    MISSING = "warning",
                    INSERT  = "info")
    type_order <- c("usage", "style", "box", "warning", "info", "error")
  } else {
    mapping <- list(SPECIAL = "i",
                    MISSING = "-",
                    INSERT  = "+")
    type_order <- c("i", "-", "+")
  }

  # which type is mapped onto which kind of logging
  relevant_types <- type_order[type_order %in% unlist(mapping)]

  # which present logging type is most important
  highest_present_type <- max(which(names(mapping) %in% unique(x)))

  return(relevant_types[highest_present_type])
}
