testthat::test_that("Get match information", {

  script <- c("mutate(dat, x = 3) %>% summarise(x == 1)",
              "filter(y == 2) %>% filter(x == 1)",
              "setDT(data)")
  lines <- seq_along(script)

  funs_prep <- "mutate|filter|summarise"
  fun_regex <- paste0("(?<!::|[[:alnum:]]|\\.|_|\\|)(",
                      funs_prep,
                      ")(?!::|%|[[:alnum:]]|\\.|_|\\|)")

  testthat::expect_equal(
    get_matches(text = script,
                line = lines,
                regex = fun_regex,
                filter_nomatches = FALSE),
    list(line = 1:3,
         string = c("mutate(dat, x = 3) %>% summarise(x == 1)",
                    "filter(y == 2) %>% filter(x == 1)",
                    "setDT(data)"),
         matches = list(c(1, 24),
                        c(1, 20),
                        -1),
         log_length = list(c(6L, 9L),
                           c(6L, 6L),
                           -1L)))

  # exclude nomatches
  testthat::expect_equal(
    get_matches(text = script,
                line = lines,
                regex = fun_regex,
                filter_nomatches = TRUE),
    list(line = 1:2,
         string = c("mutate(dat, x = 3) %>% summarise(x == 1)",
                    "filter(y == 2) %>% filter(x == 1)"),
         matches = list(c(1, 24),
                        c(1, 20)),
         log_length = list(c(6L, 9L),
                           c(6L, 6L))))

})
