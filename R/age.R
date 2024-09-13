#' Exponential age integrand
#'
#' @param x Value
#' @param lambda Rate
fexp <- function(x, lambda){
  x * lambda * exp(- lambda * x)
}

#' Exponential cdf
#'
#' @param x Value
#' @param lambda Rate
cexp <- function(x, lambda){
  1 - exp(-x * lambda)
}

#' Expected age within range assuming exponentially distributed pop
#'
#' @param age_lower Lower bound of age range (single value)
#' @param age_upper Upper bound of age range (single value)
#' @param average_age Average age of population
ea <- function(age_lower, age_upper, average_age){
  lambda <- 1 / average_age
  stats::integrate(fexp, age_lower, age_upper, lambda = lambda)$value /
    (cexp(age_upper, lambda) - cexp(age_lower, lambda))
}

#' Expected age within range assuming exponentially distributed pop
#'
#' @param age_lower Lower bound of age range (can be a vector)
#' @param age_upper Upper bound of age range (can be a vector)
#' @param average_age Average age of population
expected_age <- Vectorize(ea, vectorize.args = c("age_lower", "age_upper"))
