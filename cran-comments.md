# Submission 1.1.0

Use cli package for pretty command line outputs. RC/R6 methods are no longer
flagged as functions. Recover very long strings while using the originize_*
functions

## Test environments

* local MacOS Monterey, R 4.2.1
* tested with GitHub actions for R 3.6, R 4.1, R 4.2 on
  - Ubuntu 20.04
  - macOS
  - Windows
* rhub::check_for_cran() 
- R-hub windows-x86_64-devel (r-devel)
- R-hub ubuntu-gcc-release (r-release)
- R-hub fedora-clang-devel (r-devel)

## R CMD check results
There were no ERRORs or WARNINGs. 
There are 2 NOTEs:

* ONLY on win-builder: checking for detritus in the temp directory ... NOTE
  Found the following files/directories: 'lastMiKTeXException'. 
  This is a known Rhub issue (https://github.com/r-hub/rhub/issues/503).
* ONLY on Fedora Linux (R-hub): checking HTML version of manual ... NOTE
  Skipping checking HTML validation: no command 'tidy' found. I cannot 
  change that Tidy is not on the path, or update Tidy on the external
  Fedora Linux server. 

## Test environments
- R-hub windows-x86_64-devel (r-devel)
- R-hub ubuntu-gcc-release (r-release)
- R-hub fedora-clang-devel (r-devel)

## R CMD check results
> On windows-x86_64-devel (r-devel)
  checking for detritus in the temp directory ... NOTE
  Found the following files/directories:
    'lastMiKTeXException'

> On fedora-clang-devel (r-devel)
  checking HTML version of manual ... NOTE
  Skipping checking HTML validation: no command 'tidy' found

0 errors v | 0 warnings v | 2 notes x

