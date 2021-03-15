#' @import purrr
#' @import stats
#' @import furrr
#' @import future
#' @importFrom utils capture.output
#' @aliases blblm-package
#' @importFrom magrittr %>%
#' @details
#' Linear Regression with Little Bag of Bootstraps
"_PACKAGE"


## quiets concerns of R CMD check re: the .'s that appear in pipelines
# from https://github.com/jennybc/googlesheets/blob/master/R/googlesheets.R
utils::globalVariables(c("."))


#' bag of little bootstrap linear regression
#'
#' create a linear regression model using bag of little bootstraps, specifying the number of parts data will be split and the number of bootstrap at each split.
#' requires users to set plan prior to calling function when parallel = TRUE
#'
#' @param formula the model you would like to create
#' @param data the data you will use to pass into the function
#' @param m the number of parts you want your data to be split into.
#' @param B number of bootstraps on each m split
#' @param parallel perform function with parallel or not (default: FALSE)
#'
#' @return list of 2, B number of estimates and model fit
#' @export
blblm <- function(formula, data, m = 10, B = 5000, parallel = FALSE) {
  data_list <- split_data(data, m)
  if (parallel == TRUE) {
    estimates <- future_map(
      data_list,
      ~ lm_each_subsample(formula = formula, data = ., n = nrow(data), B = B),
      .options = furrr_options(seed = TRUE)
    )
  } else {
    estimates <- map(
      data_list,
      ~ lm_each_subsample(formula = formula, data = ., n = nrow(data), B = B)
    )
  }

  res <- list(estimates = estimates, formula = formula)
  class(res) <- "blblm"
  invisible(res)
}


#' split data into m parts of approximated equal sizes
#'
#' @param data the data you will use to pass into the function
#' @param m the number of parts you want your data to be split into.
split_data <- function(data, m) {
  idx <- sample.int(m, nrow(data), replace = TRUE)
  data %>% split(idx)
}


#' compute the estimates
#'
#' @param formula the model you would like to create
#' @param data the data you will use to pass into the function
#' @param n number of rows of the data
#' @param B number of bootstraps on each m split
lm_each_subsample <- function(formula, data, n, B) {
  # drop the original closure of formula,
  # otherwise the formula will pick a wrong variable from the global scope.
  environment(formula) <- environment()
  m <- model.frame(formula, data)
  X <- model.matrix(formula, m)
  y <- model.response(m)
  replicate(B, lm1(X, y, n), simplify = FALSE)
}


#' compute the regression estimates for a blb dataset
#'
#' @param X independent variables
#' @param y dependent varibales
#' @param n number of rows of the data
lm1 <- function(X, y, n) {
  freqs <- as.vector(rmultinom(1, n, rep(1, nrow(X))))
  fit <- lm.wfit(X, y, freqs)
  list(coef = blbcoef(fit), sigma = blbsigma(fit))
}


#' compute the coefficients from fit
#'
#' @param fit the model returned from the main blblm function
blbcoef <- function(fit) {
  coef(fit)
}


#' compute sigma from fit
#'
#' @param fit the model returned from the main blblm function
blbsigma <- function(fit) {
  p <- fit$rank
  e <- fit$residuals
  w <- fit$weights
  sqrt(sum(w * (e^2)) / (sum(w) - p))
}


#' prints blblm model
#'
#' @param x the item to be printed
#'
#' @param ... additional parameters to be passed in
#'
#' @return blblm model in string
#' @export
#' @method print blblm
print.blblm <- function(x, ...) {
  cat("blblm model:", capture.output(x$formula))
  cat("\n")
}


#' return sigma value of the models
#'
#' @param object the model returned from the main blblm function
#'
#' @param confidence TRUE or FALSE
#' @param level confidence level
#' @param ... additional parameters to be passed in
#'
#' @return double, a value of sigma after putting everything back
#'
#' @export
#' @method sigma blblm
sigma.blblm <- function(object, confidence = FALSE, level = 0.95, ...) {
  est <- object$estimates
  sigma <- mean(map_dbl(est, ~ mean(map_dbl(., "sigma"))))
  if (confidence) {
    alpha <- 1 - 0.95
    limits <- est %>%
      map_mean(~ quantile(map_dbl(., "sigma"), c(alpha / 2, 1 - alpha / 2))) %>%
      set_names(NULL)
    return(c(sigma = sigma, lwr = limits[1], upr = limits[2]))
  } else {
    return(sigma)
  }
}

#' regression model coefficients
#'
#' @param object the model returned from the main blblm function
#'
#' @param ... additional parameters to be passed in
#'
#' @return matrix of the regression model coefficients
#'
#' @export
#' @method coef blblm
coef.blblm <- function(object, ...) {
  est <- object$estimates
  map_mean(est, ~ map_cbind(., "coef") %>% rowMeans())
}


#' confidence interval of the selected independent variables
#'
#' @param object the model returned from the main blblm function
#'
#' @param parm the independent variables you want the confidence interval for
#' @param level confidence level
#' @param ... additional parameters to be passed in
#'
#' @return matrix of the lower and upper quantiles of the selected independent variables.
#'
#' @export
#' @method confint blblm
confint.blblm <- function(object, parm = NULL, level = 0.95, ...) {
  if (is.null(parm)) {
    parm <- attr(terms(object$formula), "term.labels")
  }
  alpha <- 1 - level
  est <- object$estimates
  out <- map_rbind(parm, function(p) {
    map_mean(est, ~ map_dbl(., list("coef", p)) %>% quantile(c(alpha / 2, 1 - alpha / 2)))
  })
  if (is.vector(out)) {
    out <- as.matrix(t(out))
  }
  dimnames(out)[[1]] <- parm
  out
}

#' predict the fit of the blblm model with new data
#'
#' @param object the model returned from the main blblm function
#'
#' @param new_data new data to test
#' @param confidence TRUE or FALSE
#' @param level confidence level
#' @param ... additional parameters to be passed in
#'
#' @return the fitted value of the independent variable
#' @export
#' @method predict blblm
predict.blblm <- function(object, new_data, confidence = FALSE, level = 0.95, ...) {
  est <- object$estimates
  X <- model.matrix(reformulate(attr(terms(object$formula), "term.labels")), new_data)
  if (confidence) {
    map_mean(est, ~ map_cbind(., ~ X %*% .$coef) %>%
      apply(1, mean_lwr_upr, level = level) %>%
      t())
  } else {
    map_mean(est, ~ map_cbind(., ~ X %*% .$coef) %>% rowMeans())
  }
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
