cli::test_that_cli(
  configs = c("plain", "ansi"),
  "print.pkg_usage - conflicts", {
    res <- data.frame(pkg           = "pkg1",
                      fun           = LETTERS,
                      n_calls       = 1,
                      namespaced    = FALSE,
                      conflict      = TRUE,
                      conflict_pkgs = "pkg2",
                      stringsAsFactors = FALSE)
    class(res) <- c("pkg_usage", "data.frame")
    attr(res, "pkgs") <- "pkg2"
    res
    
    testthat::local_edition(3)
    testthat::expect_snapshot({
      print(res)
    })
  })

cli::test_that_cli(
  configs = c("plain", "ansi"),
  "print.pkg_usage - conflicts - indention", {
    res <- data.frame(pkg           = rep(c("user_defined_functions",
                                            "dplyr",
                                            "data.table"),
                                          times = c(3, 3, 20)),
                      fun           = strrep(LETTERS, c(3, 4, 8)),
                      n_calls       = 1,
                      namespaced    = FALSE,
                      conflict      = TRUE,
                      conflict_pkgs = "pkg2",
                      stringsAsFactors = FALSE)
    class(res) <- c("pkg_usage", "data.frame")
    attr(res, "pkgs") <- "pkg1"
    res
    
    testthat::local_edition(3)
    testthat::expect_snapshot({
      print(res)
    })
  })

cli::test_that_cli(
  configs = c("plain", "ansi"),
  "print.pkg_usage - unknown functions", {
    res <- data.frame(pkg           = NA,
                      fun           = LETTERS,
                      n_calls       = 1,
                      namespaced    = FALSE,
                      conflict      = FALSE,
                      conflict_pkgs = NA,
                      stringsAsFactors = FALSE)
    class(res) <- c("pkg_usage", "data.frame")
    attr(res, "pkgs") <- "pkg2"
    res
    
    testthat::local_edition(3)
    testthat::expect_snapshot({
      print(res)
    })
  })

cli::test_that_cli(
  configs = c("plain", "ansi"),
  "print.pkg_usage - pkg:: usage new package", {
    res <- data.frame(pkg           = "pkg1",
                      fun           = LETTERS,
                      n_calls       = 1,
                      namespaced    = TRUE,
                      conflict      = FALSE,
                      conflict_pkgs = NA,
                      stringsAsFactors = FALSE)
    class(res) <- c("pkg_usage", "data.frame")
    attr(res, "pkgs") <- "pkg2"
    res
    
    testthat::local_edition(3)
    testthat::expect_snapshot({
      print(res)
    })
  })

cli::test_that_cli(
  configs = c("plain", "ansi"),
  "print.pkg_usage - pkg:: usage given package", {
    res <- data.frame(pkg           = "pkg1",
                      fun           = LETTERS,
                      n_calls       = 1,
                      namespaced    = TRUE,
                      conflict      = FALSE,
                      conflict_pkgs = NA,
                      stringsAsFactors = FALSE)
    class(res) <- c("pkg_usage", "data.frame")
    attr(res, "pkgs") <- "pkg1"
    res
    
    testthat::local_edition(3)
    testthat::expect_snapshot({
      print(res)
    })
  })

cli::test_that_cli(
  configs = c("plain", "ansi"),
  "print.pkg_usage - user_defined_functions", {
    res <- data.frame(pkg           = rep(c("user_defined_functions",
                                            "pkg1",
                                            "pkg123"),
                                          times = c(3, 3, 20)),
                      fun           = strrep(LETTERS, c(3, 4, 8)),
                      n_calls       = 1,
                      namespaced    = FALSE,
                      conflict      = FALSE,
                      conflict_pkgs = NA,
                      stringsAsFactors = FALSE)
    class(res) <- c("pkg_usage", "data.frame")
    attr(res, "pkgs") <- c("pkg1", "pkg123")
    res
    
    testthat::local_edition(3)
    testthat::expect_snapshot({
      print(res)
    })
  })

cli::test_that_cli(
  configs = c("plain", "ansi"),
  "print.pkg_usage - unused packages", {
    res <- data.frame(pkg           = LETTERS,
                      fun           = NA,
                      n_calls       = 0,
                      namespaced    = TRUE,
                      conflict      = FALSE,
                      conflict_pkgs = NA,
                      stringsAsFactors = FALSE)
    class(res) <- c("pkg_usage", "data.frame")
    attr(res, "pkgs") <- "pkg1"
    res
    
    testthat::local_edition(3)
    testthat::expect_snapshot({
      print(res)
    })
  })

cli::test_that_cli(
  configs = c("plain", "ansi"),
  "print.pkg_usage - used packages", {
    res <- data.frame(pkg           = LETTERS,
                      fun           = letters,
                      n_calls       = 1,
                      namespaced    = FALSE,
                      conflict      = FALSE,
                      conflict_pkgs = NA,
                      stringsAsFactors = FALSE)
    class(res) <- c("pkg_usage", "data.frame")
    attr(res, "pkgs") <- LETTERS
    res
    
    testthat::local_edition(3)
    testthat::expect_snapshot({
      print(res)
    })
  })
