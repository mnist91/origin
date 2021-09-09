## Initial submission notes

This is a new package that autamatically adds `pkg::` to function names in scripts.

The Note is for a new submission. The mentioned misspelled word are known to R users.



## Test environments

* local MacOS Mojave, R 4.1.1
* tested with GitHub actions for R 3.5, R 3.6, R 4.0 on
  - Ubuntu 18.04
  - macOS
  - Windows
* rhub::check_for_cran()
  - R-hub Windows Server 2008 R2 SP1, R-devel, 32/64 bit
  - R-hub Ubuntu Linux 20.04.1 LTS, R-release, GCC
  - R-hub Fedora Linux, R-devel, clang, gfortran


## R CMD check results

> * checking CRAN incoming feasibility ... NOTE
  New submission
  
Maintainer: 'Matthias Nistler <m_nistler@web.de>'
  
Possibly misspelled words in DESCRIPTION:

  * RStudio (13:22)
  * dplyr (9:6)
  * mtcars (8:28, 9:20)
  * lintr (13:67)
  * styler (14:9)
