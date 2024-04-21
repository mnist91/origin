# print.pkg_usage - conflicts [plain]

    Code
      print(res)
    Output
      == Package Usage Report ========================================================
      -- Used Packages: 0 ------------------------------------------------------------
      
      -- Unused Packages: 0 ----------------------------------------------------------
      
      -- Possible Namespace Conflicts:  26 -------------------------------------------
    Message
      x A    pkg1 >> pkg2
      x B    pkg1 >> pkg2
      x C    pkg1 >> pkg2
      x D    pkg1 >> pkg2
      x E    pkg1 >> pkg2
      x F    pkg1 >> pkg2
      x G    pkg1 >> pkg2
      x H    pkg1 >> pkg2
      x I    pkg1 >> pkg2
      x J    pkg1 >> pkg2
      x ... and 16 more: K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z
    Output
      
    Message
      v All used functions defined! 🥳
      

# print.pkg_usage - conflicts [ansi]

    Code
      print(res)
    Output
      [36m==[39m [1m[36mPackage Usage Report[39m[22m [36m========================================================[39m
      [36m--[39m [1mUsed Packages: 0[22m [36m------------------------------------------------------------[39m
      
      [36m--[39m [1mUnused Packages: 0[22m [36m----------------------------------------------------------[39m
      
      [36m--[39m [1mPossible Namespace Conflicts:  26[22m [36m-------------------------------------------[39m
    Message
      [31mx[39m A    pkg1 >> pkg2
      [31mx[39m B    pkg1 >> pkg2
      [31mx[39m C    pkg1 >> pkg2
      [31mx[39m D    pkg1 >> pkg2
      [31mx[39m E    pkg1 >> pkg2
      [31mx[39m F    pkg1 >> pkg2
      [31mx[39m G    pkg1 >> pkg2
      [31mx[39m H    pkg1 >> pkg2
      [31mx[39m I    pkg1 >> pkg2
      [31mx[39m J    pkg1 >> pkg2
      [31mx[39m ... and 16 more: K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z
    Output
      
    Message
      [32mv[39m All used functions defined! 🥳
      

# print.pkg_usage - conflicts - indention [plain]

    Code
      print(res)
    Output
      == Package Usage Report ========================================================
      -- Used Packages: 0 ------------------------------------------------------------
      
      -- Unused Packages: 0 ----------------------------------------------------------
      
      -- Possible Namespace Conflicts:  26 -------------------------------------------
    Message
      x AAA         user_defined_functions >> pkg2
      x BBBB        user_defined_functions >> pkg2
      x CCCCCCCC    user_defined_functions >> pkg2
      x DDD         dplyr                  >> pkg2
      x EEEE        dplyr                  >> pkg2
      x FFFFFFFF    dplyr                  >> pkg2
      x GGG         data.table             >> pkg2
      x HHHH        data.table             >> pkg2
      x IIIIIIII    data.table             >> pkg2
      x JJJ         data.table             >> pkg2
      x ... and 16 more: KKKK, LLLLLLLL, MMM, NNNN, OOOOOOOO, PPP, QQQQ, RRRRRRRR,....
    Output
      
    Message
      v All used functions defined! 🥳
      

# print.pkg_usage - conflicts - indention [ansi]

    Code
      print(res)
    Output
      [36m==[39m [1m[36mPackage Usage Report[39m[22m [36m========================================================[39m
      [36m--[39m [1mUsed Packages: 0[22m [36m------------------------------------------------------------[39m
      
      [36m--[39m [1mUnused Packages: 0[22m [36m----------------------------------------------------------[39m
      
      [36m--[39m [1mPossible Namespace Conflicts:  26[22m [36m-------------------------------------------[39m
    Message
      [31mx[39m AAA         user_defined_functions >> pkg2
      [31mx[39m BBBB        user_defined_functions >> pkg2
      [31mx[39m CCCCCCCC    user_defined_functions >> pkg2
      [31mx[39m DDD         dplyr                  >> pkg2
      [31mx[39m EEEE        dplyr                  >> pkg2
      [31mx[39m FFFFFFFF    dplyr                  >> pkg2
      [31mx[39m GGG         data.table             >> pkg2
      [31mx[39m HHHH        data.table             >> pkg2
      [31mx[39m IIIIIIII    data.table             >> pkg2
      [31mx[39m JJJ         data.table             >> pkg2
      [31mx[39m ... and 16 more: KKKK, LLLLLLLL, MMM, NNNN, OOOOOOOO, PPP, QQQQ, RRRRRRRR,....
    Output
      
    Message
      [32mv[39m All used functions defined! 🥳
      

# print.pkg_usage - unknown functions [plain]

    Code
      print(res)
    Output
      == Package Usage Report ========================================================
      -- Used Packages: 0 ------------------------------------------------------------
      
      -- Unused Packages: 0 ----------------------------------------------------------
      
      -- Functions with unknown origin: 26 -------------------------------------------
      x A     x F
      x B     x G
      x C     x H
      x D     x I
      x E     x J
    Message
      x ... and 16 more: K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z
      

# print.pkg_usage - unknown functions [ansi]

    Code
      print(res)
    Output
      [36m==[39m [1m[36mPackage Usage Report[39m[22m [36m========================================================[39m
      [36m--[39m [1mUsed Packages: 0[22m [36m------------------------------------------------------------[39m
      
      [36m--[39m [1mUnused Packages: 0[22m [36m----------------------------------------------------------[39m
      
      [36m--[39m [1mFunctions with unknown origin: 26[22m [36m-------------------------------------------[39m
      [31mx[39m A     [31mx[39m F
      [31mx[39m B     [31mx[39m G
      [31mx[39m C     [31mx[39m H
      [31mx[39m D     [31mx[39m I
      [31mx[39m E     [31mx[39m J
    Message
      [31mx[39m ... and 16 more: K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z
      

# print.pkg_usage - pkg:: usage new package [plain]

    Code
      print(res)
    Output
      == Package Usage Report ========================================================
      -- Used Packages: 0 ------------------------------------------------------------
      
      -- Unused Packages: 0 ----------------------------------------------------------
      
      -- Specifically (`pkg::fun()`) further used Packages: 1 ------------------------
      i pkg1
      
    Message
      v All used functions defined! 🥳
      

# print.pkg_usage - pkg:: usage new package [ansi]

    Code
      print(res)
    Output
      [36m==[39m [1m[36mPackage Usage Report[39m[22m [36m========================================================[39m
      [36m--[39m [1mUsed Packages: 0[22m [36m------------------------------------------------------------[39m
      
      [36m--[39m [1mUnused Packages: 0[22m [36m----------------------------------------------------------[39m
      
      [36m--[39m [1mSpecifically (`pkg::fun()`) further used Packages: 1[22m [36m------------------------[39m
      [32mi[39m pkg1
      
    Message
      [32mv[39m All used functions defined! 🥳
      

# print.pkg_usage - pkg:: usage given package [plain]

    Code
      print(res)
    Output
      == Package Usage Report ========================================================
      -- Used Packages: 1 ------------------------------------------------------------
      v pkg1
      
      -- Unused Packages: 0 ----------------------------------------------------------
      
    Message
      v All used functions defined! 🥳
      

# print.pkg_usage - pkg:: usage given package [ansi]

    Code
      print(res)
    Output
      [36m==[39m [1m[36mPackage Usage Report[39m[22m [36m========================================================[39m
      [36m--[39m [1mUsed Packages: 1[22m [36m------------------------------------------------------------[39m
      [32mv[39m pkg1
      
      [36m--[39m [1mUnused Packages: 0[22m [36m----------------------------------------------------------[39m
      
    Message
      [32mv[39m All used functions defined! 🥳
      

# print.pkg_usage - user_defined_functions [plain]

    Code
      print(res)
    Output
      == Package Usage Report ========================================================
      -- Used Packages: 2 ------------------------------------------------------------
      v pkg1  
      v pkg123
      
      -- Unused Packages: 0 ----------------------------------------------------------
      
    Message
      v All used functions defined! 🥳
      

# print.pkg_usage - user_defined_functions [ansi]

    Code
      print(res)
    Output
      [36m==[39m [1m[36mPackage Usage Report[39m[22m [36m========================================================[39m
      [36m--[39m [1mUsed Packages: 2[22m [36m------------------------------------------------------------[39m
      [32mv[39m pkg1  
      [32mv[39m pkg123
      
      [36m--[39m [1mUnused Packages: 0[22m [36m----------------------------------------------------------[39m
      
    Message
      [32mv[39m All used functions defined! 🥳
      

# print.pkg_usage - unused packages [plain]

    Code
      print(res)
    Output
      == Package Usage Report ========================================================
      -- Used Packages: 0 ------------------------------------------------------------
      
      -- Unused Packages: 13 ---------------------------------------------------------
      i A     i H
      i B     i I
      i C     i J
      i D     i K
      i E     i L
      i F     i M
      i G     
      
    Message
      v All used functions defined! 🥳
      

# print.pkg_usage - unused packages [ansi]

    Code
      print(res)
    Output
      [36m==[39m [1m[36mPackage Usage Report[39m[22m [36m========================================================[39m
      [36m--[39m [1mUsed Packages: 0[22m [36m------------------------------------------------------------[39m
      
      [36m--[39m [1mUnused Packages: 13[22m [36m---------------------------------------------------------[39m
      [33mi[39m A     [33mi[39m H
      [33mi[39m B     [33mi[39m I
      [33mi[39m C     [33mi[39m J
      [33mi[39m D     [33mi[39m K
      [33mi[39m E     [33mi[39m L
      [33mi[39m F     [33mi[39m M
      [33mi[39m G     
      
    Message
      [32mv[39m All used functions defined! 🥳
      

# print.pkg_usage - used packages [plain]

    Code
      print(res)
    Output
      == Package Usage Report ========================================================
      -- Used Packages: 26 -----------------------------------------------------------
      v A     v N
      v B     v O
      v C     v P
      v D     v Q
      v E     v R
      v F     v S
      v G     v T
      v H     v U
      v I     v V
      v J     v W
      v K     v X
      v L     v Y
      v M     v Z
      
      -- Unused Packages: 0 ----------------------------------------------------------
      
    Message
      v All used functions defined! 🥳
      

# print.pkg_usage - used packages [ansi]

    Code
      print(res)
    Output
      [36m==[39m [1m[36mPackage Usage Report[39m[22m [36m========================================================[39m
      [36m--[39m [1mUsed Packages: 26[22m [36m-----------------------------------------------------------[39m
      [32mv[39m A     [32mv[39m N
      [32mv[39m B     [32mv[39m O
      [32mv[39m C     [32mv[39m P
      [32mv[39m D     [32mv[39m Q
      [32mv[39m E     [32mv[39m R
      [32mv[39m F     [32mv[39m S
      [32mv[39m G     [32mv[39m T
      [32mv[39m H     [32mv[39m U
      [32mv[39m I     [32mv[39m V
      [32mv[39m J     [32mv[39m W
      [32mv[39m K     [32mv[39m X
      [32mv[39m L     [32mv[39m Y
      [32mv[39m M     [32mv[39m Z
      
      [36m--[39m [1mUnused Packages: 0[22m [36m----------------------------------------------------------[39m
      
    Message
      [32mv[39m All used functions defined! 🥳
      

