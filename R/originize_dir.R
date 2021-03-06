originize_dir <- function(dir,
                          pkgs = .packages(),
                          recursive = FALSE,
                          overwrite = FALSE,
                          ignoreComments = TRUE,
                          excludeBasePackages = TRUE,
                          verbose = FALSE) {
  
  
  r_files <- list.files(dir,
                        pattern = "\\.R$|\\.r", 
                        full.names = TRUE,
                        all.files = FALSE,
                        recursive = recursive)
  
  invisible(
    lapply(
      X = r_files,
      FUN = function(file) {
        originize_file(file,
                       pkgs = pkgs,
                       overwrite = overwrite,
                       ignoreComments = ignoreComments,
                       excludeBasePackages = excludeBasePackages,
                       verbose = verbose)
      }
    )
  )
  
}
