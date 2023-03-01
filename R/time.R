#' Create a new time column for aggregation
#'
#' @param x Input data.frame
#' @param time_divisor Aggregation level. Default = 1 (no aggregation).
#' Setting to 365 would allow annual aggregation, 30 monthly, 7 weekly etc.
#' @param baseline_t A baseline time to add to the time output
#'
#' @export
time_transform <- function(x, time_divisor = 1, baseline_t = 0){
  if(max(x$timestep) %% time_divisor != 0){
    warning("Number of timesteps not divisible exactly by level, group may be unequal")
  }

  x <- x |>
    dplyr::mutate(t = as.integer(ceiling(.data$timestep / time_divisor) + baseline_t)) |>
    dplyr::select(-"timestep")

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
