test_that("Default Linear Model Estimate works", {
  n <- 10000
  data <- data.frame(x = rnorm(n), y = rnorm(n))
  blblm_model = blblm(y ~ x, data)
  lm_model = lm(y~ x, data)
  expect_equal(coef(lm_model),coef(blblm_model),tolerance = .01)
})

test_that("Advance Linear Model Estimate works", {
  n <- 1000
  data <- data.frame(x = sample(1:1000,n,replace = TRUE), y = sample(1:1000,n,replace = TRUE))
  blblm_model = blblm(y ~ x, data, m = 5, B = 2500, parallel = FALSE)
  lm_model = lm(y~ x, data)
  expect_equal(coef(lm_model),coef(blblm_model),tolerance = .01)
})

test_that("Parallel Linear Model Estimate works", {
  n <- 10000
  data <- data.frame(x = sample(1:1000,n,replace = TRUE), y = sample(1:1000,n,replace = TRUE))
  plan(multiprocess, workers = 4)
  blblm_model = blblm(y ~ x, data, m = 5, B = 2500, parallel = TRUE)
  lm_model = lm(y~ x, data)
  expect_equal(coef(lm_model),coef(blblm_model),tolerance = .01)
})
