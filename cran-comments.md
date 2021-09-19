## First Resubmission Notes

Fixed description field in the DESCRIPTION file and used the MIT license template.


## Initial submission notes

This is a new package that autamatically adds `pkg::` to function names in scripts.

The Note is for a new submission. The mentioned misspelled word are known to R users.



## Test environments

* local MacOS Big Sur, R 4.1.1
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
    RStudio (12:66)
    dplyr (9:5)
    lintr (13:54)
    mtcars (8:27, 9:19)
    styler (13:64)

0 errors ✓ | 0 warnings ✓ | 1 note x