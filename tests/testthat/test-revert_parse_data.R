test_that("revert_parse_data", {
  x <- c("",
         "x <- function(x) {",
         "      mean(1:3, na.rm = TRUE)",
         "     }",
         "'Lorem ipsum dolor sit amet,\\n consectetur adipiscing elit",
         "Lorem ipsum dolor sit amet,\\n consectetur adipiscing elit",
         "Lorem ipsum dolor sit amet,\\n consectetur adipiscing elit'",
         "",
         "y <- c(1, 2, 'abc',   'mean(1:3)')",
         "lg$info('Info')",
         "mystr <- c('Lorem ipsum dolor sit amet,\\n adipiscing elit",
         "Lorem ipsum dolor sit amet,\\n consectetur adipiscing elit",
         "Lorem ipsum dolor sit amet,\\n consectetur adipiscing elit') %>% c()",
         "# trailing empty lines cannot be recovered",
         "")
  x_parsed <- get_parsed_data(x, file = "test.R")
  
  testthat::expect_equal(revert_parse_data(x_parsed),
                         expected = x[-length(x)])
  testthat::expect_equal(revert_parse_data(x_parsed,
                                           recover_empty_lines = TRUE),
                         expected = x[-length(x)])
  
  # empty lines up to the last filled input can be recovered
  testthat::expect_equal(revert_parse_data(x_parsed,
                                           recover_empty_lines = FALSE),
                         expected = x[-c(1, 8, length(x))])
})


test_that("revert_parse_data - strings > 1000 chr", {
  x <- c("'Lorem ipsum dolor sit amet, consectetur adipiscing elit,",
         "sed do eiusmod tempor incididunt ut labore et dolore",
         "magna aliqua. Venenatis tellus in metus vulputate eu",
         "scelerisque felis. Commodo elit at imperdiet dui. In ",
         "fermentum et sollicitudin ac orci phasellus egestas ",
         "tellus. Vel risus commodo viverra maecenas. Urna neque",
         "viverra justo nec. Nec dui nunc mattis enim ut. Donec",
         "pretium vulputate sapien nec sagittis. Blandit cursus",
         "risus at ultrices. Leo urna molestie at elementum eu",
         "facilisis. Dignissim convallis aenean et tortor at risus.",
         "",
         "In cursus turpis massa tincidunt dui ut. Nec feugiat",
         "in fermentum posuere. At consectetur lorem donec massa",
         "sapien faucibus et molestie. Libero enim sed faucibus ",
         "turpis in eu mi bibendum neque. Metus dictum at tempor",
         "commodo. Sed felis eget velit aliquet sagittis id",
         "consectetur. Mi sit amet mauris commodo quis imperdiet",
         "massa. Viverra vitae congue eu consequat. Tellus cras",
         "adipiscing enim eu. Id ornare arcu odio ut. Auctor neque",
         "vitae tempus quam pellentesque nec. Sodales ut etiam sit",
         "amet nisl purus. Nec feugiat in fermentum posuere urna.",
         "Urna id volutpat lacus laoreet non curabitur gravida arcu.",
         "Vel elit scelerisque mauris pellentesque pulvinar. ",
         "Convallis posuere morbi leo urna molestie at elementum eu.'"
  )
  x_parsed <- get_parsed_data(x, file = "test2.R")
  
  testthat::expect_equal(revert_parse_data(x_parsed),
                         rep("[1261 chars quoted with ''']", times = length(x)))
})