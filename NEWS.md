# origin 1.0.0
This is a new release of `origin` with significant changes and improvements. 
It adds a complete new purpose by checking the actual usage of packages in a 
project. Under the hood, parsed code rather than regexes are used to originize
code and check package usages.

### Features
- The new function `check_package_usage` takes a directory that contains R files
(sub directories included by default) and a vector of R package names. It checks
  + which (of the given) packages are **used**
  + which (of the given) packages are **not used**
  + possible **namespace conflicts** 
  + which **other packages** are used via `pkg::fct`
  + used **functions with unknown origin**
  
    Note that `check_package_usage` does **not** look for packages that
    might have exported those functions, even if the missed package has 
    been called via `::` at another occasion in the same code.
    
This functionality makes it easy to quickly check a project for which packages 
are actually still needed when dealing with a huge barrage of `library` calls.
Markers show where unspecified functions are called, either specifically 
via `pkg::` or with completely unknown origin
A detailed data.frame output, hidden under the custom `print` method, 
gives a deepdive of which functions are used how often.
  

# origin 0.5.3
- Fixed function documentation files to add missing Rd-tags
- Fixed the description field in the DESCRIPTION file to single quote 'RStudio'.
  Both fixes are to meet requirements to submit on CRAN.

# origin 0.5.2
- Update package title.
- Fix the description field in the DESCRIPTION file to not quotes only where 
  needed. Fixes are to meet requirements to submit on CRAN.
- Add a package documentation file.


# origin 0.5.1
- Fix LICENSE file to contain the CRAN MIT license template only.
- Fix the description field in the DESCRIPTION file to not include quotes /
  backticks. Both fixes are to meet requirements to submit on CRAN.
- remove outdated pkgdown files.

# origin 0.5.0
- Prepares `origin` for CRAN.
- The new function `originize_pkg` is designed to originize a package project. 
  The main difference to `originize_dir` is that it automatically considers
  all packages listed in the `DESCRIPTION` file as Imports, Suggest, or Depends.
- Define an option/argument path_to_local_functions that allows the user
  to explicitly define a folder in which functions are defined that should
  be checked for masking exported functions (e.g. a custom `mutate` function).
- replace `rprojroot` functionalities by `rstudioapi` and a logic to determine
  the shared root path of all originized files to find local functions.
- Include a custom recursive function to `get_local_functions`. Before, it was
  possible that `origin` got stuck here due to referring to symlinks.
- Cease to export `originize_text` since it should be rarely used in practice
  directly but rather internally only.
- Write a vignette to make users familiar with `origin`.

# origin 0.4.0
### Bugfixes
- HTML-code in R scripts does no longer intervene with Markers output
- improve performance by excluding project library folders (renv/packrat)
  prior to list.files rather than afterwards
- add tests for `originize_dir`


# origin 0.3.2
### Bugfixes
- Improve performance by reducing checked functions for each script.
- @ not reasonable to occur prior to a function call

# origin 0.3.1
### New
- exclude renv / packrat files automatically
- export `get_local_functions()`

### Bugfixes
- `exclude_files` did not work properly in `originize_dir()`

# origin 0.3.0
### New
- check for local function definitions that mask exported functions by using
  `origin::get_local_functions()`. It collects the names of all functions defined anywhere
  in the project, not just in the currently originized file(s). Those user 
  defined functions have higher priority than any exported function. A message
  is triggered to inform the user
  
### Chore
  - rename special functions to the
  [proper term](https://adv-r.hadley.nz/functions.html#infix-functions)
  infix functions.
  - Enhance argument descriptions in the addin `originize_current_dir()`

### Bugfixes
  - only consider R scripts for originizing via `originize_dir()`
  - extend infix function detection to containing any of the following tokens:
    % * ? ^ $ ( ) [ ] { } : = < >
  - handling of empty scripts. Misleading warning was triggered
  


# origin 0.2.2
### New
- do nothing if nothing is selected and run addin `originize selection`
- inform and ask user if many files are about to be originized
- function calls right after blanks or any of these tokens: 
  , ; = & / - < > ~ ! | ? * ^ + ( [ {


### Bugfixes
- considered default packages were only considered at the beginning of each 
  session and not updated when new packages are added to the search list

# origin 0.2.1
### New
- enhance README with showcase gif, examples, reasoning and general usage of `origin`
- Typos in function descriptions

# origin 0.2.0
### New
- get default arguments from `options()`
- insert originized text where text was selected not where the cursor currently
is

# origin 0.1.0
* First stable Version
