#' Extract basic rates from model output
#'
#' @param x Input data.frame
#' @inheritParams prevalence_format
#' @inheritParams time_transform
#'
#' @export
get_prevalence <- function(x, baseline_year = 2000, ages_as_years = TRUE){
  prevalence <- x |>
    prevalence_estimate() |>
    prevalence_format(ages_as_years = ages_as_years) |>
    format_time(baseline_year = baseline_year)
  return(prevalence)
}

#' Extract prevalence estimates from malariasimulation output
#'
#' @param x Input data.frame
prevalence_estimate <- function(x){
  prevalence_cols = colnames(x)[grepl("n_detect", colnames(x))]
  prevalence_denominator_cols = stringr::str_replace(prevalence_cols, "n_detect", "n")
  prevalence <- x[ , prevalence_cols, drop = FALSE] / x[ , prevalence_denominator_cols]
  x <- dplyr::bind_cols(timestep = x[,"timestep"], prevalence)
  return(x)
}

#' Aggregate prevalence over time
#'
#' @param x Input data.frame
prevalence_aggregate <- function(x, ...){
  x <- x |>
    dplyr::summarise(
      dplyr::across(dplyr::everything(), mean),
      time = mean(time),
      .by = dplyr::all_of(...)
    )
  return(x)
}

#' Format prevalence output
#'
#' @param x Input data.frame
#' @param age_divisor Aggregation level. For example setting to 365 will return
#' age units in years
prevalence_format <- function(x, ages_as_years){
  age_divisor = ifelse(ages_as_years, 365, 1)

  cols <- colnames(x[,])
  age_ranges <- stringr::str_split(stringr::str_replace(cols[!cols == "timestep"], "n_detect_", ""), "_")
  cols[!cols == "timestep"] <- sapply(age_ranges, function(x){
    transformed_age <- round(as.numeric(x) / age_divisor, 2)
    paste0("prevalence_", paste0(transformed_age, collapse = "_"))
  })
  colnames(x) <- cols
  return(x)
}
