## Initial submission notes

This is a new package that autamatically adds `pkg::` to function names in scripts.

The Note is for a new submission. The mentioned misspelled word are known to R users.



## Test environments

* local MacOS Mojave, R 4.1.1
* tested with GitHub actions for R 3.5, R 3.6, R 4.0 on
  - Ubuntu 18.04
  - macOS
  - Windows
  
## Test environments
- R-hub windows-x86_64-devel (r-devel)
- R-hub ubuntu-gcc-release (r-release)
- R-hub fedora-clang-devel (r-devel)

## R CMD check results
> On windows-x86_64-devel (r-devel), ubuntu-gcc-release (r-release), fedora-clang-devel (r-devel)
  checking CRAN incoming feasibility ... NOTE
  Maintainer: 'Matthias Nistler <m_nistler@web.de>'
  
  New submission
  
  Possibly misspelled words in DESCRIPTION:
    RStudio (13:22)
    dplyr (9:6)
    lintr (13:67)
    mtcars (8:28, 9:20)
    styler (14:9)

0 errors ✓ | 0 warnings ✓ | 1 note x