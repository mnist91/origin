# Packages that are natively delivered and attached by R
base_r_packages <- c("stats", "graphics", "grDevices", "datasets",
                     "utils", "methods", "base")
usethis::use_data(base_r_packages, overwrite = TRUE, internal = TRUE)
globalVariables(base_r_packages)
