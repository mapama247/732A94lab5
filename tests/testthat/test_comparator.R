context("playlists_comparator")

load("sysdata.rda")

test_that("Wrong inputs throws error", {
  expect_error(compare_countries(list("DE", "DE")))
  expect_error(compare_countries(list("CL", 1)))
  expect_error(compate_countries(list("ES", "AMXC")))
})

test_that("Check if numeric", {
	expect_equal(is.numeric(compare_countries(list("DE", "AR"))$energy), TRUE)
})