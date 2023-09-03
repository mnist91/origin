# origin - 1.1.0 <img src="misc/origin.png" width=170 align="right" />

[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/origin)](https://cran.r-project.org/package=origin)
[![metacran downloads](https://cranlogs.r-pkg.org/badges/grand-total/origin)](https://cran.r-project.org/package=origin)

| branch        | main | dev  |
| ------------- | ------ | ---- |
| R CMD check   | [![main](https://github.com/mnist91/origin/actions/workflows/r-cmd-check-fix.yml/badge.svg?branch=main)](https://github.com/mnist91/origin/actions/workflows/r-cmd-check-fix.yml) | [![dev](https://github.com/mnist91/origin/actions/workflows/r-cmd-check-fix.yml/badge.svg?branch=dev)](https://github.com/mnist91/origin/actions/workflows/r-cmd-check-fix.yml) |
| test coverage | [![main-test-coverage](https://img.shields.io/codecov/c/github/mnist91/origin/main.svg)](https://app.codecov.io/gh/mnist91/origin/branch/main) | [![dev-test-coverage](https://img.shields.io/codecov/c/github/mnist91/origin/dev.svg)](https://app.codecov.io/gh/mnist91/origin/branch/dev) |
| lints         | [![main-lints](https://github.com/mnist91/origin/actions/workflows/lints.yml/badge.svg?branch=main)](https://github.com/mnist91/origin/actions/workflows/lints.yml) | [![dev-lints](https://github.com/mnist91/origin/actions/workflows/lints.yml/badge.svg?branch=dev)](https://github.com/mnist91/origin/actions/workflows/lints.yml) |


An R package that adds `pkg::` to functions to be more explicit


To install the latest version use `remotes::install_github("mnist91/origin")`


## Overview

`origin` adds the correct package specification to all functions in your script
while giving full control about the outcome. This makes it much easier to both
convert legacy code into the `pkg::fun()` convention as well as it allows you to 
write short code first and adapt it later. 


<img src="https://raw.githubusercontent.com/mnist91/origin/dev/misc/demo_originize_file.gif" width="650px" />

Additionally, it provides an overview of all **actually** used packages in a project. 
That means, it does not only check which packages are called via `library()`,
`require()`, etc. but determines which functions from which packages are eventually used.
Useful for quickly checking a project for which packages 
are actually still needed when dealing with a huge barrage of `library` calls.

<img src="https://raw.githubusercontent.com/mnist91/origin/dev/misc/check_pkg_usage.png" width="650px" />


### Usage
#### Originize Code
To originize code either use the delivered RStudio addins or call the `origin`
functions directly, i.e. `origin::originize_file` or `origin::originize_dir`.

```
origin::originize_file("testfile.R", pkgs = c("dplyr", "data.table"))
```

#### Check Package Usage
Again, either use the delivered RStudio addin or call the function explicitly
```
origin::check_pkg_usage(path = ".",
                        pkgs = c("dplyr", "data.table"),
                        use_markes = FALSE)
```

### Settings
Most argument defaults of `origin` functions can be set via `options()`. 
This is especially useful when using the RStudio Addins.

  - `origin.pkgs`: which packages to check for functions used in the code (see **Considered Packages**).
  - `origin.ask_before_applying_changes`: whether changes should be applied
    immediately or the user must approve them first. Note that this mutes all
    checks, i.e. large number of files, local functions mask exported functions,
    and the presence and order of function conflicts.
  - `origin.overwrite`: actually insert `pkg::` into the code. Otherwise,
  logging shows only what *would* happen. Note that `ask_before_applying_changes`
  still allows to keep control over your code before `origin` changes anything.
  - `origin.check_conflicts`: should `origin` check for potential 
  namespace conflicts, i.e. a used function is defined in more than one considered
  package. User input is required to solve the issue. 
  Strongly encouraged to be set to `TRUE`.
  - `origin.add_base_packages`: should base packages also be added, e.g. `base::sum()`.
  - `origin.check_base_conflicts`: Should origin also check for conflicts
  with base R functions.
  - `origin.check_local_conflicts`: Should origin also check for conflicts
  with locally defined functions anywhere in your project? Note that it does not
  check the environment but solely parses files and scans them for function definitions
  - `origin.path_to_local_functions`: the path to the root directory of all local functions.
  defaults to the project root of the currently opened project in RStudio.
  - `origin.excluded_functions`: a (named) list of functions to exclude from checking. See details.
  - `origin.verbose`: some sort of logging is performed, either in the 
  console or via the markers tab in RStudio.
  - `origin.use_markers_for_logging`: whether to use the Markers tab in RStudio.
  - `origin.color_added_package`: hex code highlighting insertions.
  - `origin.color_missed_function`: hex code highlighting potential missings.
  - `origin.color_infix_function`: hex code highlighting infix functions (see discussion).


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

### Exclude Functions

Especially useful to solve **namespace conflicts** or ignore infix functions
like the pipe operator `%>%`. Listed functions are not considered by `origin` 
neither in adding `pkg::` nor logging. It is a list of function names. When unnamed, the 
function is generally excluded. To be more specific, a named list excludes functions
from these packages only. 

Examples:

```
# unnamed list
opitions(origin.excluded_functions = list("last", "%>%", "%<>%"))

# named list
opitions(origin.excluded_functions = list(data.table = c("last", %between%),
                                          magrittr = c("%>%", "%<>%")))

# both named and unnamed
opitions(origin.excluded_functions = list(data.table = c("last", %between%),
                                          "%>%", "%<>%"))
```
### Logging Interpretation
The logging highlights three cases:
- insertion: `pkg::` is inserted prior to a function
- missing: an object that has the same name as a function 
           but not undoubtedly used as a function. In R it is usually no problem
           to have variables that name like functions (data or df are popular examples).
           While it is always clear when a function is directly used as one, functions
           can also be arguments of other functions, most famously in functional programming 
           like the *apply family or purrr. `origin` highlights such cases in 
           the logging output.
- infix: functions like `%>%` are exported by packages but cannot be called
           with the `pkg::fun()` convention. Such functions are highlighted by default
           to point the user that these stem from a package. When using 
           dplyr-style code, consider to exclude the pipe-operator via 
           `exclude_functions`.
                                





### Discussion
Whether or not to add `pkg::` to each (imported) function is a [controversial](https://stackoverflow.com/q/4372145/8107362)
[issue](https://stackoverflow.com/q/23232791/8107362) in the R community. While the tidyverse style guide does not mention explicit namespacing, [R Packages](https://r-pkgs.org) and the [Google R style guide](https://google.github.io/styleguide/Rguide.html#qualifying-namespaces) are in favor of it.

Pros

+ very explicit
+ completely avoid namespace conflicts
+ no need to attach the complete namespace of a package
+ keep track of which function belongs to which package

Cons 

- (minimal) performance issue
- more writing required
- longer code
- infix functions like `%>%` cannot be called via `magrittr::%>%`
and workarounds are still required here. Either use 
  ```
  library(magrittr, include.only = "%>%")
  `%>%` <- magrittr::`%>%`
  ```
- calling `library()` on top of a script clearly indicates which packages are
  needed. A not yet installed package throws an error right away, not until
  a function cannot be found later in the script. However, one can use 
  the `include_only` argument and set it to `NULL`. No functions are attached
  into the search list then.
  ```
  library(magrittr, include_only = NULL)
  ``` 
