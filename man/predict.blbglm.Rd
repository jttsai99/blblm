% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/blbglm.R
\name{predict.blbglm}
\alias{predict.blbglm}
\title{predict the fit of the blbglm model with new data}
\usage{
\method{predict}{blbglm}(object, new_data, confidence = FALSE, level = 0.95, ...)
}
\arguments{
\item{object}{the model returned from the main blbglm function}

\item{new_data}{new data to test}

\item{confidence}{boolean to include confidence interval}

\item{level}{confidence level between 0 and 1}

\item{...}{additional parameters to be passed in}
}
\value{
the fitted value of the independent variable
}
\description{
predict the fit of the blbglm model with new data
}
