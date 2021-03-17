# test_that("Default Logistic Model Estimate works", {
#   # generate data, n has to be greater to be more accurate
#   n <- 1000
#   data <- data.frame(x = sample(1:1000, n, replace = TRUE), y = sample(0:1, n, replace = TRUE))
#
#   # fit model by blblm with default parameters
#   blglm_model <- blglm(y ~ x, data)
#
#   # fit model by lm
#   lm_model <- lm(y ~ x, data)
#
#   # compare lm to its bootstrap estimate
#   expect_equal(coef(lm_model), coef(blblm_model), tolerance = .1)
# })