
context('variance')

test_that('create variance', {
  var <- as.variance(1e-5)
  expect_equal(as.numeric(var), 1e-5)
})

test_that('fail creating annualized variance', {
  expect_error(as.variance(1e-3, TRUE))
})

test_that('create annualized variance', {
  var <- as.variance(1e-3, TRUE, 252)
  expect_equal(as.numeric(var), 1e-3)
})

test_that('annualize variance', {
  var <- as.variance(1e-5)
  var_annu <- annualize(var)
  expect_equal(as.numeric(var_annu), 1e-5*252)
  
  var_annu <- as.variance(1e-5*252, TRUE, 252)
  var_annu <- annualize(var_annu)
  expect_equal(as.numeric(var_annu), 1e-5*252)
})

test_that('convert to daily variance', {
  var <- as.variance(1e-3, TRUE, 252)
  var_daily <- daily(var)
  expect_equal(as.numeric(var_daily), 1e-3/252)
})

test_that('convert to volatility', {
  var <- as.variance(1e-5)
  vol <- as.volatility(var)
  expect_equal(as.numeric(vol), sqrt(as.numeric(var)))
  
  var <- as.variance(1e-3, TRUE, 252)
  vol <- as.volatility(var)
  expect_equal(as.numeric(vol), sqrt(as.numeric(var)))
})




