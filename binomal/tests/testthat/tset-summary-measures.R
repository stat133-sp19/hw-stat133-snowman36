context("Check summary measures")

test_that("aux_mean", {
  expect_equal(aux_mean(10,0.3), 3)
  expect_equal(aux_mean(10,0.5), 5)
  expect_equal(aux_mean(10,0.4), 4)
})

test_that("aux_avriance", {
  expect_equal(aux_variance(10.0.3), 2.1)
  expect_equal(aux_variance(20.0.5), 5)
  expect_equal(aux_variance(30.0.4), 7.2)
})

test_that("aux_mode", {
  expect_equal(aux_mode(10,0.3), 3)
  expect_equal(aux_mode(10,0.5), 0.5)
  expect_equal(aux_mode(20,0.4), 8)
})

test_that("aux_skewness", {
  expect_equal(aux_skewness(10,0.3), 0.2760262)
  expect_equal(aux_skewness(10,0.8), -0.4743416)
  expect_equal(aux_skewness(20,0.7), -0.19518)
})

test_that("aux_kurtosis", {
  expect_equal(aux_kurtosis(10,0.3), -0.1238095)
  expect_equal(aux_kurtosis(10,0.5), -0.2)
  expect_equal(aux_kurtosis(30,0.7), -0.04126984)
})
