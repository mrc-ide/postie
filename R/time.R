#' Adds time columns
#'
#' Adds year (assuming no leap years), month, week (assuming last week in the year is 8 days),
#' day and time columns. Time is usuful for plotting.
#'
#' @param x Input data.frame
#' @param baseline_year baseline_year Baseline year
format_time <- function(x, baseline_year){

  days_in_months <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
  months <- rep(1:12, days_in_months)
  weeks <- c(rep(1:52, each = 7), 52)[1:365]

  time <- baseline_year + (x$timestep - 1) / 365
  year <- floor(time)
  day <- 1 + ((x$timestep - 1) %% 365)
  month <- months[day]
  week <- weeks[day]

  x <- x |>
    dplyr::mutate(
      time = time,
      year = year,
      month = month,
      week = week,
      day = day
    ) |>
    dplyr::select(time, year, month, week, day, dplyr::everything())

  return(x)
}

#' Remove burn in period from simulation output
#'
#' @param x Input data.frame
#' @param burnin Length of burn in period in days
#'
#' @export
drop_burnin <- function(x, burnin){
  if(burnin >= max(x$timestep)){
    stop("burn in period must be < the maximum timestep")
  }
  if(burnin < 0){
    stop("burn in period must be positive")
  }

  x <- x[x$timestep > burnin, ]
  x$timestep <- as.integer(x$timestep - burnin)
  return(x)
}
