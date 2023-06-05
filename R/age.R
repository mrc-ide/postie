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
#' @param lower_age Lower bound of age range (single value)
#' @param upper_age Upper bound of age range (single value)
#' @param average_age Average age of population
ea <- function(lower_age, upper_age, average_age){
  lambda <- 1 / average_age
  stats::integrate(fexp, lower_age, upper_age, lambda = lambda)$value /
    (cexp(upper_age, lambda) - cexp(lower_age, lambda))
}

#' Expected age within range assuming exponentially distributed pop
#'
#' @param lower_age Lower bound of age range (can be a vector)
#' @param upper_age Upper bound of age range (can be a vector)
#' @param average_age Average age of population
expected_age <- Vectorize(ea, vectorize.args = c("lower_age", "upper_age"))
