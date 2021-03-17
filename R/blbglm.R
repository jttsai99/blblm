#' @import purrr
#' @import stats
#' @import furrr
#' @import future
#' @importFrom utils capture.output
#' @importFrom magrittr %>%
#' @details
#' Logistic Regression with Little Bag of Bootstraps
"_PACKAGE"


## quiets concerns of R CMD check re: the .'s that appear in pipelines
# from https://github.com/jennybc/googlesheets/blob/master/R/googlesheets.R
utils::globalVariables(c("."))

#' @export
blglm <- function(formula, data, m = 5, B = 1000, parallel = FALSE) {
  data_list <- split_data(data, m)
  if (parallel == TRUE) {
    estimates <- future_map(
      data_list,
      ~ glm_each_subsample(formula = formula, data = ., n = nrow(data), B = B),
      .options = furrr_options(seed = TRUE)
    )
  } else {
    estimates <- map(
      data_list,
      ~ glm_each_subsample(formula = formula, data = ., n = nrow(data), B = B)
    )
  }

  res <- list(estimates = estimates, formula = formula)
  class(res) <- "blglm"
  invisible(res)
}



#' split data into m parts of approximated equal sizes
#'
#' @param data the data you would like to analyze.
#' @param m the number of sub-samples you want to divide your data into.
split_data <- function(data, m) {
  idx <- sample.int(m, nrow(data), replace = TRUE)
  data %>% split(idx)
}



#' This computes estimates for the partitioned sub-samples from split_data.
glm_each_subsample <- function(formula, data, n, B) {
  replicate(B, glm_each_boot(formula, data, n), simplify = FALSE)
}

#' Under BLB, this computes regression estimates.
glm_each_boot <- function(formula, data, n) {
  freqs <- rmultinom(1, n, rep(1, nrow(data)))
  glm1(formula, data, freqs)
}


#' creates the glm model for each repetition and determines the coefficients and sigmas of each BLB.
glm1 <- function(formula, data, freqs) {
  # drop the original closure of formula,
  # otherwise the formula will pick wrong variables from a parent scope.
  environment(formula) <- environment()
  fit <- glm(formula, data, weights = freqs, family = "binomial")
  list(coef = blbcoef(fit), sigma = blbsigma(fit))
}


#' compute the coefficients from fit
#'
#' @param fit the model returned from the blglm function
blbcoef <- function(fit) {
  coef <- fit$coefficients
}


#' compute sigma from fit
#'
#' @param fit the model returned from the blglm function
blbsigma <- function(fit) {
  p <- fit$rank
  e <- fit$residuals
  w <- fit$weights
  sqrt(sum(w * (e^2)) / (sum(w) - p))
}


#' prints blglm model
#'
#' @param x the item to be printed
#'
#' @param ... additional parameters to be passed in
#'
#' @return NULL
#' @export
#' @method print blglm
print.blglm <- function(x, ...) {
  cat("blglm model:", capture.output(x$formula))
  cat("\n")
}

sigma.blglm <- function(object, confidence = FALSE, level = 0.95, ...) {
  est <- object$estimates
  sigma <- mean(map_dbl(est, ~ mean(map_dbl(., "sigma"))))
  if (confidence) {
    alpha <- 1 - level
    limits <- est %>%
      map_mean(~ quantile(map_dbl(., "sigma"), c(alpha / 2, 1 - alpha / 2))) %>%
      set_names(NULL)
    return(c(sigma = sigma, lwr = limits[1], upr = limits[2]))
  } else {
    return(sigma)
  }
}

coef.blglm <- function(object, ...) {
  est <- object$estimates
  map_mean(est, ~ map_cbind(., "coef") %>% rowMeans())
}








mean_lwr_upr <- function(x, level = 0.95) {
  alpha <- 1 - level
  c(fit = mean(x), quantile(x, c(alpha / 2, 1 - alpha / 2)) %>% set_names(c("lwr", "upr")))
}

map_mean <- function(.x, .f, ...) {
  (map(.x, .f, ...) %>% reduce(`+`)) / length(.x)
}

map_cbind <- function(.x, .f, ...) {
  map(.x, .f, ...) %>% reduce(cbind)
}

map_rbind <- function(.x, .f, ...) {
  map(.x, .f, ...) %>% reduce(rbind)
}
