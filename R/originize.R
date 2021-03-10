#' Add Explicit Package Names to its Functions
#'
#' @param file a path to a script
#' @param pkgs a vector with package names
#' @param overwrite a boolean, if TRUE the file will be saved and overwritten. If FALSE the file is returned.
#' @param ignoreComments a boolean, if TRUE lines starting with # are ignored
#' @param verbose a boolean
#'
#' @return
#' @export
#'
originize <- function(script,
                      file,
                      functions,
                      pkgs = .packages(),
                      overwrite = FALSE,
                      ignoreComments = TRUE,
                      verbose = FALSE,
                      html = TRUE) {
  
  
  # get relevant function information
  l <- checkFunctions(script = script,
                      functions = unlist(functions),
                      verbose = verbose,
                      ignoreComments = ignoreComments)
  
  # iterate over all functions and find position where package:: is necessary
  replacement_list <- 
    Map(f = function(pkg, funs) {
      get_origins(pkg            = pkg,
                  script         = script,
                  functions      = funs,
                  file           = file,
                  overwrite      = overwrite,
                  ignoreComments = ignoreComments,
                  verbose        = verbose)
    },
    pkgs,
    functions
    )
  
  # keep package info only if matches are present
  replacement_list <- Filter(function(l) length(l) > 0, replacement_list)
  
  # combine results for all packages
  combined <- Reduce(f = function(...) {
    Map(f = c,
        ...)
  },
  x = replacement_list)
  
  # insert package:: to the relevant positions
  fixed_lines_list <- lapply(
    X = sort(unique(combined$line)), 
    FUN = prep_line_originize,
    combined = combined)
  
  # combine all lines
  fixed_lines_dat <- Reduce(rbind, fixed_lines_list)
  
  
  # if no logging is desired, skpi all relevant steps
  if (!verbose) {
    script[fixed_lines_dat$line] <- fixed_lines_dat$string
    
    return(list(to_write = list(file = file, script = script),
                logging_data = data.frame()))
    
  } else {
    
    # get positions of potential missined (special) functions
    potential_missings <- prep_verbose(script = script,
                                       line_matches = l$line_matches,
                                       functions = unlist(functions),
                                       functions_in_script = l$functions_in_script,
                                       special_functions = l$special_functions,
                                       special_matches = l$special_matches)
    
    # combine positions of potential missings
    logging_comb <-  Reduce(
      f = function(...) {
        Map(f = c,
            ...)
      }, 
      x = c(replacement_list,
            Filter(f = Negate(is.null), 
                   x = potential_missings)))
    
    
    # prepare strings and insert color highlighting where needed
    fixed_lines_list <- lapply(
      X = sort(unique(logging_comb$line)), 
      FUN = prep_line_logging,
      logging_comb = logging_comb,
      html = html)
    
    # combine all lines
    logging_data <- Reduce(rbind, fixed_lines_list)
    
    
    
    
    if (html) {
      # add further attributes for markers output
      # include other types?
      # c("usage", "error", "warning", "info", "style", "box")
      logging_data$type <- "usage"
      logging_data$file <- file
      logging_data$column <- 1
      attr(logging_data$message, which = "class") <- c("html", "character")
    }
    script[fixed_lines_dat$line] <- fixed_lines_dat$string
    
    return(list(to_write = list(file = file, script = script),
                logging_data = logging_data))
  }
  
}
