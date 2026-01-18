# check_pkg_usage() working on test

    Code
      print(res)
    Output
      == Package Usage Report ========================================================
      -- Used Packages: 2 ------------------------------------------------------------
      v data.table
      v testthat  
      
      -- Unused Packages: 0 ----------------------------------------------------------
      
      -- Functions with unknown origin: 8 --------------------------------------------
      x bind_cols       x map        
      x bind_cols_2     x mutate     
      x filter          x n          
      x if_else         x n_distinct 

# check_pkg_usage working on target

    Code
      res
    Output
      == Package Usage Report ========================================================
      -- Used Packages: 2 ------------------------------------------------------------
      v data.table
      v testthat  
      
      -- Unused Packages: 0 ----------------------------------------------------------
      
      -- Specifically (`pkg::fun()`) further used Packages: 2 ------------------------
      i dplyr
      i purrr
      
      -- Functions with unknown origin: 1 --------------------------------------------
      x bind_cols_2

