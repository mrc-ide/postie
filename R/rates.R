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
  if(age_divisor < 1){
    stop("age_divisor must be >= 1")
  }
  clinical_cols <- colnames(x)[grepl("inc_clinical", colnames(x))]
  severe_cols <- colnames(x)[grepl("inc_sev", colnames(x))]
  denominator_cols <- stringr::str_replace(clinical_cols, "_inc_clinical", "")

  if(length(clinical_cols) != length(severe_cols)){
    stop("The same age_min and age_max must be provided for clinical and severe incidence")
  }
  rates <- x |>
    time_transform(
      time_divisor = time_divisor,
      baseline_t = baseline_t
    ) |>
    rates_time_aggregate(
      clinical_cols = clinical_cols,
      severe_cols = severe_cols,
      denominator_cols = denominator_cols
    ) |>
    rates_format(
      clinical_cols = clinical_cols,
      severe_cols = severe_cols,
      age_divisor = age_divisor
    ) |>
    treatment_scaling(
      treatment_scaler = treatment_scaler,
      baseline_treatment = baseline_treatment
    ) |>
    mortality_rate(
      scaler = scaler
    ) |>
    dplyr::select(c("t", "age_lower", "age_upper", "clinical", "severe", "mortality", "n", "prop_n"))

  if(aggregate_age){
    rates <- rates |>
      rates_age_aggregate()
  }
  return(rates)
}

#' Aggregate rates output over t
#'
#' @param x Input data.frame
#' @param denominator_cols Names of denominators columns
#' @inheritParams rates_format
rates_time_aggregate <- function(x, clinical_cols, severe_cols, denominator_cols){
  x |> dplyr::summarise(
    dplyr::across(dplyr::all_of(clinical_cols), sum),
    dplyr::across(dplyr::all_of(severe_cols), sum),
    dplyr::across(dplyr::all_of(denominator_cols), mean),
    ft = mean(.data$ft),
    .by = "t"
  )
}

#' Format rates
#'
#' Create rates from counts and specifies age output units
#'
#' @param x Input data.frame
#' @param clinical_cols Clinical incidence column names
#' @param severe_cols Severe incidence column names
#' @param age_divisor Aggregation level. For example setting to 365 will return
#' age units in years
rates_format <- function(x, clinical_cols, severe_cols, age_divisor){
    x |>
      dplyr::rename_with( ~ gsub("n_inc_", "", .x, fixed = TRUE), .cols = dplyr::all_of(clinical_cols)) |>
      dplyr::rename_with( ~ gsub("n_inc_", "", .x, fixed = TRUE), .cols = dplyr::all_of(severe_cols)) |>
      tidyr::pivot_longer(
        cols = -c("t", "ft")
      ) |>
    tidyr::separate_wider_delim(.data$name, "_", names = c("name", "age_lower", "age_upper")) |>
    tidyr::pivot_wider(id_cols = c("t", "ft", "age_lower", "age_upper"), names_from = "name", values_from = "value") |>
    dplyr::mutate(
      clinical = .data$clinical / .data$n,
      severe = .data$severe / .data$n,
      age_lower = round(as.numeric(.data$age_lower) / age_divisor),
      age_upper = round(as.numeric(.data$age_upper) / age_divisor),
      n = round(.data$n)
    )|>
    dplyr::mutate(prop_n = .data$n / sum(.data$n), .by = "t")
}


#' Aggregate rates output over age
#'
#' @param x Input data.frame
rates_age_aggregate <- function(x){
  x <- x |>
    dplyr::summarise(
      clinical = stats::weighted.mean(.data$clinical, .data$prop_n),
      severe  = stats::weighted.mean(.data$severe, .data$prop_n),
      mortality = stats::weighted.mean(.data$mortality, .data$prop_n),
      n = sum(.data$n),
      .by = "t"
    )
  return(x)
}

