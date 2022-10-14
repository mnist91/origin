devtools::load_all()
dplyr::mutate(iris, x = 3)

originize_selection(pkgs = "dplyr", overwrite = TRUE)
originize_file(file.path("inst", "testpath", "file1.R"),
               pkgs = "dplyr",
               verbose = TRUE,
               ask_before_applying_changes = TRUE,
               use_markers = FALSE)
originize_dir(path = file.path("inst", "testpath"),
              pkgs = "dplyr",
              verbose = TRUE,
              ask_before_applying_changes = TRUE)

iris %>%
  dplyr::filter(Species == "setosa") %>%
  dplyr::filter(Spepal.Length > 3) %>%
  dplyr::filter(TRUE)
