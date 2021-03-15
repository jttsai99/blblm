test_that("Basic Linear Model Estimate works", {
  n <- 10000
  data <- data.frame(x = rnorm(n), y = rnorm(n))
  blblm_model = blblm(y ~ x, data)
  lm_model = lm(y~ x, data)
  expect_equal(coef(lm_model),coef(blblm_model),tolerance = .01)
})
