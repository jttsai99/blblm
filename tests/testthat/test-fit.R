test_that("Default Linear Model Estimate works", {
  # generate data, n has to be greater to be more accurate
  n <- 1000
  data <- data.frame(x = rnorm(n), y = rnorm(n))

  # fit model by blblm with default parameters
  blblm_model <- blblm(y ~ x, data)

  # fit model by lm
  lm_model <- lm(y ~ x, data)

  # compare lm to its bootstrap estimate
  expect_equal(coef(lm_model), coef(blblm_model), tolerance = .1)
})


test_that("Advance Linear Model Estimate works", {
  # generate data
  n <- 1000
  data <- data.frame(x = sample(1:1000, n, replace = TRUE), y = sample(1:1000, n, replace = TRUE))

  # fit model by blblm with different parameters
  blblm_model <- blblm(y ~ x, data, m = 5, B = 2500, parallel = FALSE)

  # fit model by lm
  lm_model <- lm(y ~ x, data)

  # compare lm to its bootstrap estimate
  expect_equal(coef(lm_model), coef(blblm_model), tolerance = .01)
})

test_that("Parallel Linear Model Estimate works", {
  # generate data
  n <- 10000
  data <- data.frame(x = sample(1:1000, n, replace = TRUE), y = sample(1:1000, n, replace = TRUE))

  # setup workers for parallel
  plan(multiprocess, workers = 4)

  # fit model by blblm with different parameters and utilize multiple cpu
  blblm_model <- blblm(y ~ x, data, m = 5, B = 2500, parallel = TRUE)

  # fit model by lm
  lm_model <- lm(y ~ x, data)

  # compare lm to its bootstrap estimate
  expect_equal(coef(lm_model), coef(blblm_model), tolerance = .01)
})

test_that("Parallel providing faster speed works", {
  # generate large data
  n <- 10000
  data <- data.frame(x = sample(1:1000, n, replace = TRUE), y = sample(1:1000, n, replace = TRUE))

  # setup workers for parallel
  plan(multiprocess, workers = 4)

  # run with multiple cpu (parallel) and get elapse time
  parallel_speed <- system.time(blblm(y ~ x, data, m = 5, B = 5000, parallel = TRUE))[3]

  # run without parallel
  regular_speed <- system.time(blblm(y ~ x, data, m = 5, B = 5000, parallel = FALSE))[3]

  # parallel time is shorter than regular time
  expect_lte(parallel_speed, regular_speed)
})
