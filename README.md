# origin - 0.1.0 <img src="misc/origin.png" width=170 align="right" />


| branch        | main | dev  |
| ------------- | ------ | ---- |
| R CMD check   | [![main](https://github.com/mnist91/origin/actions/workflows/r-cmd-check-fix.yml/badge.svg?branch=main)](https://github.com/mnist91/origin/actions/workflows/r-cmd-check-fix.yml) | [![dev](https://github.com/mnist91/origin/actions/workflows/r-cmd-check-fix.yml/badge.svg?branch=dev)](https://github.com/mnist91/origin/actions/workflows/r-cmd-check-fix.yml) |
| test coverage | [![main-test-coverage](https://img.shields.io/codecov/c/github/mnist91/origin/main.svg)](https://codecov.io/gh/mnist91/origin/branch/main) | [![dev-test-coverage](https://img.shields.io/codecov/c/github/mnist91/origin/dev.svg)](https://codecov.io/gh/mnist91/origin/branch/dev) |
| lints         | [![main-lints](https://github.com/mnist91/origin/actions/workflows/lints.yml/badge.svg?branch=main)](https://github.com/mnist91/origin/actions/workflows/lints.yml) | [![dev-lints](https://github.com/mnist91/origin/actions/workflows/lints.yml/badge.svg?branch=dev)](https://github.com/mnist91/origin/actions/workflows/lints.yml) |


An R package that adds `package::` to functions to be more explicit


To install the latest version use `remotes::install_github("mnist91/origin")`


## Overview

`origin` adds the correct package specification to all functions in your script
while giving full control about the outcome. This makes it much easier to both
convert legacy code into the `package::` convention as well as it allows you to 
write short code first and adapt it later. 


### Usage
To originize code either use the delivered RStudio addins or call the `origin`
functions directly, i.e. `origin::originize_file` or `origin::originize_dir`.

```
origin::originize_file("testfile.R", pkgs = c("dplyr", "data.table"))
```

### Settings
Most argument defaults of `origin` functions can be set via `options()`. 
This is especially usefull when using the RStudio Addins.

  - `origin.pkgs`: which packages to check for functions used in the code
  - `origin.ask_before_applying_changes`: whether changes should be applied
  immediately or the user must approve them first
  - `origin.use_markers_for_logging`: whether to use the Markers tab in RStudio
  - `origin.color_added_package`: hex code highlighting insertions
  - `origin.color_missed_function`: hex code highlighting potential missings
  - `origin.color_special_function`: hex code highlighting special functions (see discussion)
  - `origin.overwrite = TRUE`: actually insert `pkg::` into the code. Otherwise,
  logging shows only what *would* happen. Note that `ask_before_applying_changes`
  still allows to keep control over your code before origin changes anything.
  - `origin.ignore_comments = TRUE`: should comments be ignored
  - `origin.check_conflicts = TRUE`: should `origin` check for potential 
  namespace conflicts, i.e. a used function is defined in more than one considered
  package. User input is required to solve the issue. 
  Strongly encouraged to be set to `TRUE`.
  - `origin.check_base_conflicts = TRUE`: Should origin also check for conflicts
  with base R functions.
  - `origin.add_base_packages = FALSE`: should base packages also added e.g., `base::sum()`
  - `origin.excluded_functions = list()`: a list of functions to exclude from checking. See details.
  - `origin.verbose = TRUE`: some sort of logging is performed, either in the 
  console or via the markers tab in RStudio.


### Considered Packages
By default, `orgigin` considers all attached packages as given by `.packages()` 
except the standard R packages (`base`, `methods`, `stats`, `utils`, `graphics`, 
`datasets`). For the current list of loaded packages also check `search()`.
Note that, in case of namespace conflicts, the order in the search list 
determines which namespace masks which. `origin` uses the same rule as R, i.e.
the latest loaded package masks the other packages. Therefore, in case
there is a potential namespace conflict in your code, the changes made by 
`origin` should yield the same result as before but being more explicit
about it. Since this can break code functionality, `origin` issues a warning and 
user input is required. 

To overwrite the default just use a character vector of package names.

### 




### Discussion
Whether or not to add `package::` to each (imported) function is a [controversial](https://stackoverflow.com/q/4372145/8107362)
issue in the R community. 

Pros

+ very explicit
+ completely avoid namespace conflicts
+ no need to attach the complete namespace of a package
+ keep track of which function belongs to which package

Cons 

- (minimal) performance issue
- more writing required
- longer code
- special functions like `%between%` cannot be called via `data.table::%between%`
and workarounds are still required here. Either use 
  ```
  library(data.table, include.only = "%between%")
  `%between%` <- data.table::`%between%`
  ```
- calling `library()` on top of a script clearly indicates which packages are
  needed. A not yet installed package throws an error right away, not until
  a function cannot be found later in the script. However, one can use 
  the `include_only` argument and set it to `NULL`. No functions are loaded
  into the namesapce then.
  ```
  library(data.table, include_only = NULL)
  ``` 
