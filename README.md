# origin - 0.1.0 <img src="misc/origin.png" width=170 align="right" />


| branch        | main | dev  |
| ------------- | ------ | ---- |
| R CMD check   | [![main](https://github.com/mnist91/origin/actions/workflows/r-cmd-check-fix.yml/badge.svg?branch=main)](https://github.com/mnist91/origin/actions/workflows/r-cmd-check-fix.yml) | [![dev](https://github.com/mnist91/origin/actions/workflows/r-cmd-check-fix.yml/badge.svg?branch=dev)](https://github.com/mnist91/origin/actions/workflows/r-cmd-check-fix.yml) |
| test coverage | [![main-test-coverage](https://img.shields.io/codecov/c/github/mnist91/origin/main.svg)](https://codecov.io/gh/mnist91/origin/branch/main) | [![dev-test-coverage](https://img.shields.io/codecov/c/github/mnist91/orign/dev.svg)](https://codecov.io/gh/mnsit91/origin/branch/dev) |
| lints         | [![main-lints](https://github.com/mnist91/origin/actions/workflows/lints.yml/badge.svg?branch=main)](https://github.com/mnist91/origin/actions/workflows/lints.yml) | [![dev-lints](https://github.com/mnist91/origin/actions/workflows/lints.yml/badge.svg?branch=dev)](https://github.com/mnist91/origin/actions/workflows/lints.yml) |


An R package that adds package:: to functions to be more explicit


To install the latest version use `remotes::install_github("mnist91/origin")`


## Overview

`origin` adds the correct package specification to all functions in your script
while giving full control about the outcome. This makes it much easier to both
convert legacy code to the `package::` convention as well as it allows you to 
write short code first and adapt it later. 


## Usage
To originize code either use the delivered RStdudio addins or call the `origin`
functions directly, i.e. `origin::originize_file` or `origin::originize_dir`.

```
origin::originize_file("testfile.R", pkgs = c("dplyr", "data.table"))

```

### Settings
Most argument defaults of `origin` functions can be set via `options()`.

  - `origin.pkgs`: which packages to check for functions used in the code
  - `origin.ask_before_applying_changes`: whether changes should be applied
  immediately or the user must approve them first
  - `origin.use_markers_for_logging`: whether to use the Markers tab in Rstudio
  - `origin.color_added_package`: hex code highlighting insertions
  - `origin.color_missed_function`: hex code highlighting potential missings
  - `origin.color_special_function`: hex code highlighting special functions (see discussion)

#### Packages
By default, `orgigin` considers all loaded packages as given by `.packages()` 
except base R packages (`base`, `methods`, `stats`, `utils`, `graphics`, 
`datasets`). For the current list of loaded packages also check `search()`.
Note that, in case of namespace conflicts, the order in the search list 
determines which namespace maskes which. `origin` uses the same rule as R
that the latest loaded package maskes the other packages. Therefore, in case
there is a potential namespace conflict in your code, the changes made by 
`origin` should yield the same result as before but beeing more explicit
about it. Since this can break code functionality, `origin` issues a warning and 
user input is required. 




### Discussion
Whether ot not to add `package::` to each (imported) function is a controversial
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
and work arounds are still required here. Either use 
```
library(data.table, include.only = "%between%")
`%between%` <- data.table::`%between%`
```