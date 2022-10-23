get_function_calls <- function(dat, exclude_namespaced_funs = TRUE) {
  
  # positions of function taking arguments
  symbol_sub_token <- which(
    dat$token == "SYMBOL_SUB" &
      # typical parameter names that take functions as inputs
      # NOT: this list is NOT exhaustive!
      dat$text %in% c("FUN", ".f", "f"))
  # positions of parameter assignment operators
  argument_token <- which(dat$token == "EQ_SUB")
  # positions of assigned symbols. These CAN be functions
  symbol_token <- which(dat$token == "SYMBOL")
  
  # a possible function assigned to a parameter must be...
  # ... preceeded by a parameter assignment operator
  relevant_symbols <- symbol_token[symbol_token %in% (argument_token + 1)]
  # ... preceeded by a typical parameter name
  relevant_symbols <- 
    relevant_symbols[relevant_symbols %in% (symbol_sub_token + 2)]
  
  if (length(relevant_symbols) > 0) {
    dat[relevant_symbols, "usage"] <- "FUNCTION_CALL"
  }

  # regular function calls, like detected by parsing or code highlighting
  fct_symbols <- which(dat$token %in% c("SYMBOL_FUNCTION_CALL", "SYMBOL"))
  
  # exclude functions that are already qualified by a namespace
  if (exclude_namespaced_funs) {
    ns_symbols <- which(dat$token %in% c("NS_GET", "NS_GET_INT"))
    fct_with_ns <- fct_symbols %in% (ns_symbols + 1)
    namespaced_fct_symbols <- fct_symbols[fct_with_ns]
    fct_symbols <- fct_symbols[!fct_with_ns]
    if (length(namespaced_fct_symbols) > 0) {
      dat[namespaced_fct_symbols, "usage"] <- "NAMESPACED_FUNCTION_CALL"
  }
}
  
  if (length(fct_symbols) > 0) {
    dat[dat$token == "SYMBOL_FUNCTION_CALL" &
          is.na(dat$usage), 
        "usage"] <- "FUNCTION_CALL"
  }
  
  return(dat)
}
