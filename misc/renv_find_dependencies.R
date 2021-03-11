debugonce(renv:::renv_snapshot_r_packages_impl)
debugonce(renv:::snapshot)
debugonce(renv:::renv_snapshot_filter)
debugonce(renv:::renv_snapshot_filter_impl)
debugonce(renv:::renv_snapshot_dependencies)
debugonce(renv:::renv_dependencies_impl)
renv::snapshot()

body(renv::snapshot)


test <- renv_snapshot_r_packages(libpaths = libpaths, 
                                 project = project) %>% 
  renv_snapshot_filter(project = project, 
                       type = type, packages = packages) %>% 
  renv_snapshot_fixup()


sum(names(records) == "readr")

reprex::reprex({
  mypath <- "/Users/matthiasnistler/Projekte/2021/origin" #getwd()
  finds <- renv:::dependencies(path = mypath, root = mypath)
  finds[1:5,]
  unique(finds$Package)
  
})


finds <- renv:::dependencies(path = getwd(), root = getwd())
#> Finding R package dependencies ... Done!
finds[1:5,]
#>                                                       Source    Package Require
#> 1    /Users/matthiasnistler/Projekte/2021/origin/DESCRIPTION rstudioapi        
#> 2    /Users/matthiasnistler/Projekte/2021/origin/DESCRIPTION   testthat      >=
#> 3 /Users/matthiasnistler/Projekte/2021/origin/dev/01_start.R    usethis        
#> 4   /Users/matthiasnistler/Projekte/2021/origin/dev/02_dev.R   devtools        
#> 5   /Users/matthiasnistler/Projekte/2021/origin/dev/02_dev.R rstudioapi        
#>   Version   Dev
#> 1         FALSE
#> 2   3.0.0 FALSE
#> 3         FALSE
#> 4         FALSE
#> 5         FALSE
unique(finds$Package)
#>  [1] "rstudioapi"   "testthat"     "usethis"      "devtools"     "goodpractice"
#>  [6] "inteRgrate"   "pkgdown"      "rhub"         "crayon"       "purrr"       
#> [11] "renv"         "data.table"   "dplyr"        "origin"