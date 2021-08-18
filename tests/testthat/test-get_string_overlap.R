testthat::test_that("Regular Strings Overlap", {
  x <- c("aabbcc", "aabbdcc", "aabccc", "aabc", "aabbbbbbbbbbbb")
  testthat::expect_equal(get_string_overlap(x),
                         "aab")

  # with NAs
  x <- c("aabbcc", "aabbdcc", "aabccc", NA, "aabc", "aabbbbbbbbbbbb")
  testthat::expect_equal(get_string_overlap(x),
                         "aab")

  # with empty Strings
  x <- c("aabbcc", "aabbdcc", "aabccc", "", "aabc", "aabbbbbbbbbbbb")
  testthat::expect_equal(get_string_overlap(x),
                         "")
})


testthat::test_that("Path-like Strings Overlap", {
  # Begin Exclude Linting
  x <- c("/var/folders/dr/7rd1jt_d7kn9zf3g52cfmqd40000gp/T//RtmpteYqx4/file6dff44743d40/targetfile1.R",
         "/var/folders/dr/7rd1jt_d7kn9zf3g52cfmqd40000gp/T//RtmpteYqx4/file6dff44743d40/targetfile2.R",
         "/var/folders/dr/7rd1jt_d7kn9zf3g52cfmqd40000gp/T//RtmpteYqsx4/file6dff44743d40/testfile1.R",
         "/var/folders/dr/7rd1jt_d7kn9zf3g52cfmqd40000gp/T//RtmpteYqx4/file6dff44743dx40/testfile2.R",
         "/var/folders/dr/7rd1jt_d7kn9zf3g52cfmqd40000gp/T//RtmpteYqx4/file6dff44743d40/testfile_empty.R")
  # End Exclude Linting

  testthat::expect_equal(get_string_overlap(x),
                         "/var/folders/dr/7rd1jt_d7kn9zf3g52cfmqd40000gp/T//RtmpteYq") # Exclude Linting

})


testthat::test_that("Empty Strings", {
  x <- c(NA, NA, NA)
  testthat::expect_equal(get_string_overlap(x),
                         NA_character_)

  # with NAs
  x <- c("", "", "")
  testthat::expect_equal(get_string_overlap(x),
                         "")

})
