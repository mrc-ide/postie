#' Extract basic rates from model output
#'
#' @param diagnostic Can be light microscopy ("lm"), or PCR ("pcr")
#' @inheritParams get_rates
#'
#' @export
get_prevalence <- function(x, diagnostic = "lm", baseline_year = 2000, ages_as_years = TRUE){
  stopifnot(diagnostic %in% c("lm", "pcr"))
  prevalence <- x |>
    prevalence_estimate(diagnostic = diagnostic) |>
    prevalence_format(diagnostic = diagnostic, ages_as_years = ages_as_years) |>
    format_time(baseline_year = baseline_year) |>
    dplyr::select(-"timestep") |>
    dplyr::select("year", "month", "week", "day", "time", dplyr::everything())
  return(prevalence)
}

#' Extract prevalence estimates from malariasimulation output
#'
#' @inheritParams get_prevalence
prevalence_estimate <- function(x, diagnostic){
  name <- paste0("n_detect_", diagnostic)
  prevalence_cols = colnames(x)[grepl(name, colnames(x))]
  prevalence_denominator_cols = stringr::str_replace(prevalence_cols, name, "n_age")
  prevalence <- x[ , prevalence_cols, drop = FALSE] / x[ , prevalence_denominator_cols]
  x <- dplyr::bind_cols(timestep = x[,"timestep"], prevalence)
  return(x)
}

#' Aggregate prevalence over time
#'
#' @inheritParams get_prevalence
#' @param ... Aggregation columns
prevalence_aggregate <- function(x, ...){
  x <- x |>
    dplyr::summarise(
      dplyr::across(dplyr::everything(), mean),
      time = mean(.data$time),
      .by = dplyr::all_of(...)
    )
  return(x)
}

#' Format prevalence output
#'
#' @inheritParams get_prevalence
prevalence_format <- function(x, diagnostic, ages_as_years){
  name <- paste0("n_detect_", diagnostic, "_")
  age_divisor = ifelse(ages_as_years, 365, 1)

  cols <- colnames(x[,])
  age_ranges <- stringr::str_split(stringr::str_replace(cols[!cols == "timestep"], name, ""), "_")
  cols[!cols == "timestep"] <- sapply(age_ranges, function(x){
    transformed_age <- round(as.numeric(x) / age_divisor, 2)
    paste0(diagnostic, "_prevalence_", paste0(transformed_age, collapse = "_"))
  })
  colnames(x) <- cols
  return(x)
}
