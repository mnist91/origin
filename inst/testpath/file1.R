devtools::load_all()
mutate(iris, x = 3)

originize_selection(pkgs = "dplyr", overwrite = TRUE)
originize_file("inst/testpath/file1.R", pkgs = "dplyr", verbose = TRUE, ask_before_applying_changes = TRUE, use_markers = F)
originize_dir("inst/testpath", pkgs = "dplyr", verbose = TRUE, ask_before_applying_changes = TRUE)