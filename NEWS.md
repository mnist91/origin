# origin 0.5.0

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
  - Enahnce arguemnt descriptipns in the addin `originize_current_dir()`

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
- considered defualt packages were only considered at the beginning of each 
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
