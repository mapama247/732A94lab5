context("playlists_comparator")

library(spotifyr)
Sys.setenv(SPOTIFY_CLIENT_ID = 'b5c4a61095b74ce597f886697704ea3c')
Sys.setenv(SPOTIFY_CLIENT_SECRET = '6b2271624f0c49a9abb65255b84cb462')
access_token <- get_spotify_access_token()

load("sysdata.rda")

test_that("Check if correct inputs", {
  expect_error(compare_countries(list("CL", 1)))
  expect_error(compate_countries(list("ES", "AMXC")))
})

test_that("Check if numeric outputs", {
	expect_equal(is.numeric(compare_countries(list("DE", "AR"))$energy), TRUE)
	expect_equal(is.numeric(get_followers()), TRUE)
	expect_equal(is.numeric(get_explicit()), TRUE)
})