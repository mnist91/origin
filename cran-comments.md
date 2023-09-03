# Submission 1.1.1

Fix package version comparison to character in one test file to account for the
requirement by CRAN and _R_CHECK_STOP_ON_INVALID_NUMERIC_VERSION_INPUTS_=true.

## Test environments

* local MacOS Monterey, R 4.3.1
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
There are 3 NOTEs:

* ONLY on win-builder: checking for detritus in the temp directory ... NOTE
  Found the following files/directories: 'lastMiKTeXException'. 
  This is a known Rhub issue (https://github.com/r-hub/rhub/issues/503).
* ONLY on win-builder:   checking for non-standard things in the check directory ... NOTE
  Found the following files/directories: ''NULL''
  This is a known Rhub issue (https://github.com/r-hub/rhub/issues/560).
* On Fedora and Ubuntu Linux (R-hub): checking HTML version of manual ... NOTE
  Skipping checking HTML validation: no command 'tidy' found. I cannot 
  change that Tidy is not on the path, or update Tidy on the external
  Fedora Linux server. 

## Test environments
- R-hub windows-x86_64-devel (r-devel)
- R-hub ubuntu-gcc-release (r-release)
- R-hub fedora-clang-devel (r-devel)

## R CMD check results
> On windows-x86_64-devel (r-devel)
  checking for non-standard things in the check directory ... NOTE
  Found the following files/directories:
    ''NULL''

> On windows-x86_64-devel (r-devel)
  checking for detritus in the temp directory ... NOTE
  Found the following files/directories:
    'lastMiKTeXException'

> On ubuntu-gcc-release (r-release), fedora-clang-devel (r-devel)
  checking HTML version of manual ... NOTE
  Skipping checking HTML validation: no command 'tidy' found

0 errors v | 0 warnings v | 3 notes x

