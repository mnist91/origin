# Submission 1.1.2

Fix that RStudio color highlighting in the markers pane is no longer posssible
via HTML but ansi colours.

## Test environments

* local MacOS Monterey, R 4.3.3
* tested with GitHub actions for R 3.6, R 4.0, R 4.2 on
  - Ubuntu 20.04
  - macOS
  - Windows
* rhub::rhub_check() 
  - R-hub [VM] linux
  - R-hub [VM] windows
  - R-hub [VM] macos
  - R-hub [VM] macos-arm64
  - R-hub [CT] ubuntu-release
  - R-hub [CT] ubuntu-next

## R CMD check results
See https://github.com/mnist91/origin/actions/runs/8773339195.
There were no ERRORs, WARNINGs, or Notes
