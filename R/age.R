# Define the function for the integrand
integrand <- function(x, rate){
  x * stats::dexp(x, rate)
  }

#' Expected age within range assuming exponentially distributed pop
#'
#' @param lower_age Lower bound of age range (single value)
#' @param upper_age Upper bound of age range (single value)
#' @param average_age Average age of population
ea <- function(lower_age, upper_age, average_age){
  rate <- 1 / average_age
  stats::integrate(integrand, lower_age, upper_age, rate = rate)$value /
    (stats::pexp(upper_age, rate) - stats::pexp(lower_age, rate))
}

#' Expected age within range assuming exponentially distributed pop
#'
#' @param lower_age Lower bound of age range (can be a vector)
#' @param upper_age Upper bound of age range (can be a vector)
#' @param average_age Average age of population
expected_age <- Vectorize(ea, vectorize.args = c("lower_age", "upper_age"))
