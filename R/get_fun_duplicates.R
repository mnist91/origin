# named character vector of functions with package name as names
get_fun_duplicates <- function(functions) {
  funs_unlisted <- un_list(functions, nms = names(functions))

  funs_duplicates <- funs_unlisted[which(duplicated(funs_unlisted))]
  dups <- funs_unlisted[funs_unlisted %in% funs_duplicates]

  return(dups)
}
