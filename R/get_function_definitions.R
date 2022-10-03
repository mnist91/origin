get_function_definitions <- function(dat) {
  # extract token positions
  symbol_token <- which(dat$token %in% "SYMBOL")
  assign_token <- which(dat$token %in% c("EQ_ASSIGN", "LEFT_ASSIGN"))
  fun_token <- which(dat$token %in% "FUNCTION")
  anonym_fun_token1 <- which(dat$token %in% c("'\\\\'"))
  anonym_fun_token2 <- which(dat$token %in% c("'('"))
  
  # symbol must be prior to assignment AND
  relevant_symbols <- symbol_token[symbol_token %in% (assign_token - 1)]
  relevant_symbols <- relevant_symbols[
    relevant_symbols %in% (
      # assignment must be followed by a function call OR
      c(assign_token[assign_token %in% (fun_token - 1)],
        # followed by an anonymous function which consists of
        # 1. a backslash
        assign_token[assign_token %in% (anonym_fun_token1 - 1) &
                       # 2. an opening bracket
                       assign_token %in% (anonym_fun_token2 - 2)
        ]) - 1)]
  
  # in case function definitions are detected
  if (length(relevant_symbols) > 0) {
    dat[relevant_symbols, "usage"] <- "FUNCTION_DEFINITION"
  }
  return(dat)
}