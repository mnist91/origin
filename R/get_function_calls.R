get_function_calls <- function(dat) {

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
  relevant_symbols <- relevant_symbols[relevant_symbols %in% (symbol_sub_token + 2)]
  dat[relevant_symbols, "usage"] <- "FUNCTION_CALL"

  # regular function calls, like detected by parsing or code highlighting
  dat[dat$token == "SYMBOL_FUNCTION_CALL", "usage"] <- "FUNCTION_CALL"

  return(dat)
}
