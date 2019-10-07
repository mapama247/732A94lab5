context("playlists_comparator")
#library(spotifyr)
#library(installr)

test_that("Wrong inputs throws error", {
  expect_error(compare_countries(list("DE", "DE")))
  expect_error(compare_countries(list("CL", 1)))
  expect_error(compate_countries(list("ES", "AMXC")))
})

test_that("Check if numeric", {
  expect_equal(compare_countries(is.numeric(avg_features$energy)), TRUE)
  
})