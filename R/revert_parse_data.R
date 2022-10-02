revert_parse_data <- function(parse_data, recover_empty_lines = TRUE){
  res <- by(data = parse_data,
            # consider each line separately
            # TODO: exclude loop
            INDICES = list(parse_data$line1, parse_data$file), 
            FUN = function(args) {
              # determine number of tokens in the line
              n_cases <- length(args$text)
              
              # determine width of white space needed prior to each token
              # it is its starting position minus 
              # the end position of the precessing token minus 1 
              # since positions are including
              n_ws <- args$col1 - c(0, args$col2[-n_cases]) - 1
              
              # create needed whitespace
              ws <- strrep(" ", n_ws)
              
              # add whitespaces to the tokens
              paste(ws, args$text, collapse = "", sep = "")
            })
  
  # recover empty lines if needed
  if (recover_empty_lines){
    out <- character(max(parse_data$line1))
    out[unique(parse_data$line1)] <- res
    return(out)
  } else {
    return(res)
  }
  
}
