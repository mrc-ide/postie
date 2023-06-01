#' Extract basic rates from model output
#'
#' @param x Input data.frame
#' @inheritParams rates_format
#' @inheritParams time_transform
#' @inheritParams mortality_rate
#' @inheritParams treatment_scaling
#' @param aggregate_age Aggregate output over age groups
#'
#' @export
get_rates <- function(x, time_divisor, baseline_t, age_divisor, scaler, treatment_scaler, baseline_treatment, aggregate_age = FALSE){
  rates <- x |>
    rates_column_check() |>
    rates_transform() |>
    time_transform(time_divisor = time_divisor, baseline_t = baseline_t) |>
    rates_time_aggregate() |>
    rates_format(age_divisor = age_divisor) |>
    treatment_scaling(treatment_scaler = treatment_scaler, baseline_treatment = baseline_treatment) |>
    mortality_rate(scaler = scaler)

  if(aggregate_age){
    rates <- rates |>
      rates_age_aggregate()
  }
  return(rates)
}

#' Rates input checks
#'
#' Checks that the required columns are present and that the age-ranges for
#' required columns align.
#'
#' @param x Input data.frame
rates_column_check <- function(x){
  cols <- colnames(x)
  if(!"timestep" %in% cols){
    stop("required column `timestep` missing")
  }
  if(!"ft" %in% cols){
    stop("required column `ft` missing")
  }
  if(sum(grepl("n_inc_clinical", cols)) == 0){
    stop("required columns `n_inc_clinical_...` missing")
  }
  if(sum(grepl("n_inc_severe", cols)) == 0){
    stop("required columns `n_inc_severe_...` missing")
  }
  if(sum(grepl("n_age_", cols)) == 0){
    stop("required columns `n_age_...` missing")
  }
  clinical_cols <- cols[grepl("n_inc_clinical", cols)]
  clinical_age_ranges <- stringr::str_split(stringr::str_replace(clinical_cols, "n_inc_clinical_", ""), "_")
  severe_cols <- cols[grepl("n_inc_severe", cols)]
  severe_age_ranges <- stringr::str_split(stringr::str_replace(severe_cols, "n_inc_severe_", ""), "_")
  n_age_cols <- cols[grepl("n_age", cols)]
  n_age_ranges <- stringr::str_split(stringr::str_replace(n_age_cols, "n_age_", ""), "_")
  if(!identical(clinical_age_ranges, severe_age_ranges) | !identical(clinical_age_ranges, n_age_ranges)){
    stop("Age ranges for `n_inc_clinical_...` and `n_inc_severe_...` and `n_age_...` outputs must be the same")
  }
  return(x)
}

#' Transform rates into long form
#'
#' @param x Input data.frame
rates_transform <- function(x){
  x <- x |>
    dplyr::select(
      "timestep",
      "ft",
      dplyr::contains("n_age"),
      dplyr::contains("n_inc_clinical"),
      dplyr::contains("n_inc_severe"),
    ) |>
    tidyr::pivot_longer(
      cols = -c("timestep", "ft")
    ) |>
    dplyr::mutate(
      name = stringr::str_remove(.data$name, "n_inc_"),
      name = stringr::str_remove(.data$name, "n_")
    )

  x_names <- strsplit(x$name, "_")
  x$name <- sapply(x_names, "[", 1)
  x$age_lower <- as.numeric(sapply(x_names, "[", 2))
  x$age_upper <- as.numeric(sapply(x_names, "[", 3))

  x <- x |>
    tidyr::pivot_wider(
      id_cols = c("timestep", "age_lower", "age_upper", "ft")
    )
  return(x)
}

#' Aggregate rates output over t
#'
#' @param x Input data.frame
rates_time_aggregate <- function(x){
  x <- x |>
    dplyr::summarise(
      dplyr::across(c("clinical", "severe"), sum),
      dplyr::across(c("age", "ft"), mean),
      .by = c("t", "age_lower", "age_upper")
    )
  return(x)
}


#' Format rates
#'
#' Create rates from counts and specifies age output units
#'
#' @param x Input data.frame
#' @param age_divisor Aggregation level. For example setting to 365 will return
#' age units in years
rates_format <- function(x, age_divisor = 365){
  if(age_divisor < 1){
    stop("age_divisor must be > 1")
  }

  x <- x |>
    dplyr::mutate(
      prop_age = .data$age / sum(.data$age),
      .by = "t"
    ) |>
    dplyr::mutate(
      clinical = .data$clinical / .data$age,
      severe = .data$severe / .data$age,
      age_lower = round(.data$age_lower / age_divisor),
      age_upper = round(.data$age_upper / age_divisor)) |>
    dplyr::select(-"age")
  return(x)
}

#' Aggregate rates output over age
#'
#' @param x Input data.frame
rates_age_aggregate <- function(x){
  x <- x |>
    dplyr::summarise(
      clinical = stats::weighted.mean(.data$clinical, .data$prop_age),
      severe  = stats::weighted.mean(.data$severe, .data$prop_age),
      mortality = stats::weighted.mean(.data$mortality, .data$prop_age),
      .by = "t"
    )
  return(x)
}

