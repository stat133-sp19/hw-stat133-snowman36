context("check function")

test_that("check_prob", {
  expect_true(check_prob(prob = 0.5))
  expect_true(check_prob(prob = 1))
  expect_error(check_prob(prob = -5))
})

test_that("check_trials", {
  expect_true(check_trials(trials = 50))
  expect_error(check_trials(trials = -10))
  expect_error(check_trials(trials = -5))
})

test_that("check_success", {
  expect_true(check_success(success = c(4,5,6),trials = 50))
  expect_error(check_success(success = -55, trials = 50))
  expect_error(check_success(success = 50, trials = 20))
})
