context('volatility')

test_that('create volatility', {
  vol <- as.volatility(1e-3)
  expect_equal(as.numeric(vol), 1e-3)
})

test_that('fail creating annualized volatility', {
  expect_error(as.volatility(1e-3, TRUE))
})

test_that('create annualized volatility', {
  vol <- as.volatility(1e-2, TRUE, 252)
  expect_equal(as.numeric(vol), 1e-2)
})

test_that('annualize volatility', {
  vol <- as.volatility(1e-3)
  vol_annu <- annualize(vol)
  expect_equal(as.numeric(vol_annu), 1e-3*sqrt(252))
  
  vol_annu <- as.volatility(1e-3*sqrt(252), TRUE, 252)
  vol_annu <- annualize(vol_annu)
  expect_equal(as.numeric(vol_annu), 1e-3*sqrt(252))
})

test_that('convert to daily volatility', {
  vol <- as.volatility(1e-3, TRUE, 252)
  vol_daily <- daily(vol)
  expect_equal(as.numeric(vol_daily), 1e-3/sqrt(252))
})

test_that('convert to variance', {
  vol <- as.volatility(1e-3)
  var <- as.variance(vol)
  expect_equal(as.numeric(var), as.numeric(vol)^2)
  
  vol <- as.volatility(1e-3, TRUE, 252)
  var <- as.variance(vol)
  expect_equal(as.numeric(var), as.numeric(vol)^2)
})

