# Submission 1.2.1

Due to impact on a new dplyr version, examples and tests are adapted. Prior to
the change, I used dplyr::id() in examples. This function is going to be removed
so my checks would fail eventually and hamper their release. So I changed
from dplyr::id to dplyr::filter in the showcases.

## Test environments

* local MacOS Monterey, R 4.5.2
* tested with GitHub actions for R 4.3.3, R 4.4.2, R 4.5.2 on
  - Ubuntu 20.04
  - macOS
  - Windows
* rhub::rhub_check() 
  - R-hub [VM] linux
  - R-hub [VM] windows
  - R-hub [CT] ubuntu-release
  - R-hub [CT] ubuntu-next

## R CMD check results
https://github.com/mnist91/origin/actions/runs/21115809141
There were no ERRORs, WARNINGs, or NOTEs

## Win Devel check results
https://win-builder.r-project.org/38Is2lw28Fuk/00check.log
There were no ERRORs, WARNINGs, or NOTEs