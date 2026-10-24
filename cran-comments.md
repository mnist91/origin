# Submission 1.2.2

Due to an - apparent - update in getParseData, a text had to be adapted to
mirror its new behavior to *not* strip information after 1000 characters by 
`[1234 chars quoted with ''']` as it was before.

## Test environments

* local MacOS Golden Gate, R 4.6.1
* tested with GitHub actions for R 4.3.3, R 4.4.2, R 4.6.1 on
  - Ubuntu 20.04
  - macOS
  - Windows
* rhub::rhub_check() 
  - R-hub [VM] linux
  - R-hub [VM] windows
  - R-hub [CT] ubuntu-release
  - R-hub [CT] ubuntu-next

## R CMD check results
https://github.com/mnist91/origin/actions/runs/40125164201
There were no ERRORs, WARNINGs, or NOTEs

## Win Devel check results
https://win-builder.r-project.org/3HwE7p5891H7/
There were no ERRORs, WARNINGs, or NOTEs