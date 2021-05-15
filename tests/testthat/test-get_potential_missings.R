testthat::test_that("Find potential missed functions", {

  # setup
  script <- c("mutate(dat, x = 3) %>% summarise(x == 1)",
              "filter(y == 2) %>% filter(x == 1) %>% .",
              "setDT(data)")
  line_matches <- rep(TRUE, length(script))
  funs <- c("filter", "mutate", "%>%", "summarise")
  funs_script <- c("filter", "mutate", "summarise")
  funs_infix <- 3L
  funs_spec_matches <- TRUE

  # with infix matches
  testthat::expect_equal(
    get_potential_missings(script = script,
                           line_matches = line_matches,
                           functions = funs,
                           functions_in_script = funs_script,
                           infix_functions = funs_infix,
                           infix_matches = funs_spec_matches),
    list(infixes = list(line = 1:2,
                         string = c("mutate(dat, x = 3) %>% summarise(x == 1)",
                                    "filter(y == 2) %>% filter(x == 1) %>% ."),
                         matches = list(20, c(16, 35)),
                         log_length = list(3L, c(3L, 3L)),
                         pkg = c("", ""),
                         type = c("infix", "infix")),
         pot_missings = list(line = 1:2,
                             string = c("mutate(dat, x = 3) %>% summarise(x == 1)",
                                        "filter(y == 2) %>% filter(x == 1) %>% ."),
                             matches = list(c(1, 24), c(1, 20)),
                             log_length = list(c(6L, 9L), c(6L, 6L)),
                             pkg = c("", ""),
                             type = c("missed",  "missed")))
  )

  # without infixes
  funs_spec_matches <- FALSE
  testthat::expect_equal(
    get_potential_missings(script = script,
                           line_matches = line_matches,
                           functions = funs,
                           functions_in_script = funs_script,
                           infix_functions = funs_infix,
                           infix_matches = funs_spec_matches),
    list(infixes = NULL,
         pot_missings = list(line = 1:2,
                             string = c("mutate(dat, x = 3) %>% summarise(x == 1)",
                                        "filter(y == 2) %>% filter(x == 1) %>% ."),
                             matches = list(c(1, 24), c(1, 20)),
                             log_length = list(c(6L, 9L), c(6L, 6L)),
                             pkg = c("", ""),
                             type = c("missed",  "missed")))
  )

})
