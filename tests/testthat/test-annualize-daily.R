
context('annualize and daily')

test_that('convert volatility from annual to daily and vice-versa', {
  vol_d <- as.volatility(1e-3)
  vol_a <- as.volatility(1e-3*sqrt(252), TRUE, 252)
  expect_equal(annualize(vol_d), vol_a)
  expect_equal(daily(vol_a), vol_d)
})

test_that('convert variance from annual to daily and vice-versa', {
  var_d <- as.variance(1e-5)
  var_a <- as.variance(1e-5*252, TRUE, 252)
  expect_equal(annualize(var_d), var_a)
  expect_equal(daily(var_a), var_d)
})