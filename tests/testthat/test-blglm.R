test_that("Default Logistic Model Estimate works", {
  # generate data, n has to be greater to be more accurate
  n <- 1000
  data <- data.frame(x = sample(1:1000, n, replace = TRUE), y = sample(0:1, n, replace = TRUE))

  # fit model by blblm with default parameters
  blbglm_model <- blbglm(y ~ x, data)

  # fit model by lm
  log_model <- glm(y ~ x, data,family = "binomial")

  # compare lm to its bootstrap estimate
  expect_equal(coef(log_model), coef(blbglm_model), tolerance = .1)
})

# test_that("Parallel providing faster speed works", {
#   # generate large data
#   n <- 10000
#   data <- data.frame(x = sample(1:1000, n, replace = TRUE), y = sample(0:1, n, replace = TRUE))
#
#   # setup workers for parallel
#   plan(multiprocess, workers = 4)
#
#   # run with multiple cpu (parallel) and get elapse time
#   parallel_speed <- system.time(blbglm(y ~ x, data, m = 5, B = 2000, parallel = TRUE))[3]
#
#   # run without parallel
#   regular_speed <- system.time(blbglm(y ~ x, data, m = 5, B = 2000, parallel = FALSE))[3]
#
#   # parallel time is shorter than regular time
#   expect_lte(parallel_speed, regular_speed)
# })