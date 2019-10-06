context("playlists_comparator")

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