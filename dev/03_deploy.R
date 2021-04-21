######################################
#### CURRENT FILE: DEPLOY SCRIPT #####
######################################

# Test your app

## Run checks ----
## Check the package before sending to prod
devtools::check()
rhub::check_for_cran()

covr::codecov(token = "b044750c-7f42-4875-a95c-2e8a3c3a694b")
covr::package_coverage(path = ".")
usethis::use_version(which = "minor")

# Roxygen
devtools::document()
pkgdown::build_site()

# Tests
devtools::test()
devtools::test_coverage()

# Check your examples in your manuals
# unless you set your examples to \dontrun{} or \donttest{})
devtools::run_examples()

# Spell check
devtools::spell_check()

# Run goodpractice check
goodpractice::gp()

# Check package dependencies
inteRgrate::check_pkg()

# Check if code adheres to standards
inteRgrate::check_lintr()

# Check if your description is tidy
inteRgrate::check_tidy_description()

# Check if file names are correct
inteRgrate::check_r_filenames()

# Check if .gitignore contains standard files
inteRgrate::check_gitignore()

# Update cran-comments.md
devtools::check()

# check win devel
devtools::check_win_devel()

# Check for CRAN specific requirements using rhub and save it in the results
# objects
results <- rhub::check_for_cran()

# Get the summary of your results
# takes a while to get
results$cran_summary()

# Build Package
devtools::build()
