## 3rd Resubmission Notes
- Fixed quoting of RStudio
- Added missing Value Rd-tags
- Regarding the missing arguments-tag inside of get_pkgs_from_description.Rd:
  This function does not have any arguments. Base R functions like R.Version()
  does not have any arguments either and the Arguments-tag is missing there, as
  well. Is it okay to not use this tag in such a case or what is the proposed
  solution for such functions without inputs?

## 2nd Resubmission Notes
- Fixed quoting in the description field in the DESCRIPTION file  
- Updated the package title

- Regarding the general uefulness of using the double colon operator '::': 
The Google R Style guide encourages users to explicitly qualify namespaces for
all external functions
(https://google.github.io/styleguide/Rguide.html#qualifying-namespaces).

Also "R Packages" by Hadley Wickham says:
"It’s common for packages to be listed in Imports in DESCRIPTION, but not in 
NAMESPACE. In fact, this is what I recommend: list the package in DESCRIPTION 
so that it’s installed, then always refer to it explicitly with pkg::fun().
Unless there is a strong reason not to, it’s better to be explicit. It’s a 
little more work to write, but a lot easier to read when you come back to the 
code in the future. The converse is not true."
https://r-pkgs.org/namespace.html#imports

Finally, not all R projects are in fact packages, hence do not have a NAMESPACE.
It is sometimes much easier to understand unknown code if it is clear from which 
package a function stems from.


## 1st Resubmission Notes
Fixed description field in the DESCRIPTION file and used the MIT license template.


## Initial submission notes

This is a new package that autamatically adds `pkg::` to function names in scripts.

The Note is for a new submission. The mentioned misspelled word are known to R users.



## Test environments

* local MacOS Big Sur, R 4.1.1
* tested with GitHub actions for R 3.5, R 3.6, R 4.0 on
  - Ubuntu 20.04
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

0 errors ✓ | 0 warnings ✓ | 1 note x
