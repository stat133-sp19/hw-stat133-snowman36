context("check binomial")

test_that("bin_choose", {
  expect_equal(bin_choose(5,2), 10)
  expect_error(bin_choose(3,5))
  expect_equal(bin_choose(5, 1:3), c(5,10,10))
})

test_that("bin_probability", {
  expect_equal(bin_probability(2,5,0.5), 0.3125)
  expect_error(bin_probability(7,5,0.5))
  expect_equal(bin_probability(0:2, 5, 0.5), c(0.03125,0.15625,0.31250))
})

test_that("bin_distribution", {
  expect_is(bin_distribution(5,0.5), "bindis")
  expect_length(bin_distribution(5,0.5), 2)
  expect_length(bin_distribution(5,0.5)$success, 6)
})

test_that("bin_cumulative", {
  expect_is(bin_cumulative(5,0.5), "bincum")
  expect_length(bin_cumulative(5,0.5), 3)
  expect_length(bin_cumulative(5,0.5)$success, 6)
})

