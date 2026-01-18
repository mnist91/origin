# from an rmd/qmd file, return which contain chunks of r code (logical vector)
extract_r_chunk_lines <- function(script) {
  
  # detect lines that mark the beginning/end of a chunk
  # Chunks begin with a line start with ```{r...} 
  # and end with a line that is just ```
  # all lines between these triple backticks is the r chunk
  chunk_lines <- startsWith(script, "```")
  
  # mark the lines that belong one chunk
  chunk_grps <- cumsum(chunk_lines)
  
  # r code is all uneven chunkgroup that is not the begin (```{r...}) of 
  # the chunk
  r_lines <- !chunk_lines & chunk_grps %% 2 == 1
  
  return(r_lines)
}

# from an rmd/qmd file, return chunks of r code (character vector)
extract_r_chunks <- function(script) {
  
  r_lines <- extract_r_chunk_lines(script)
  
  # extract the lines
  out <- script[r_lines]
  return(out)
}

# check if a file is an R-Markdown/Quarto file
is_rmd_file <- function(file) {
  grepl(x = file,
        pattern = "\\.RMD$|\\.qmd$",
        ignore.case = TRUE)
}

# make a pattern out of givven filetypes.
# additionally check, if the belong to a supported subset
make_filetype_pattern <- function(filetypes) {
  with_point <- startsWith(filetypes, prefix = ".")
  if (any(with_point)) {
    filetypes[with_point] <- sub(pattern = ".",
                                 replacement = "",
                                 x = filetypes[with_point],
                                 fixed = TRUE)
  }
  filetypes <- unique(toupper(filetypes))
  
  # currently supported filetypes
  supported_filetypes <- c("R", "RMD", "QMD")
  filetype_error <- !filetypes %in% supported_filetypes
  if (any(filetype_error)) {
    stop("Currently supported filetypes are ",
         paste0(".", supported_filetypes, collapse = ", "),
         ". Tried to originize ",
         paste0(".", filetypes[filetype_error], collapse = ", "))
  }
  
  # R becomes \\.R$
  pattern <- paste0("\\.", filetypes, "$", collapse = "|")
  
  return(pattern)
}
