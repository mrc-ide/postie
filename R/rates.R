#' Extract basic rates from malariasimulation model output
#'
#' Summarises clinical incidence `clinical`, severe incidence `severe` and mortality `mortality`
#' rates as well as per-capita years lived with disability `yld_pp`, years of life
#' lost `yll_pp` and daly `dalys_pp`.
#'
#' For DALYs: disability weights are from the Global Burden of Disease study. To estimate
#' YLL we assume the average life expectancy for a person aged x years taken from the UN WPP
#' Africa profile for 2022. To estimate the expected age in a given range we assume
#' exponentially distributed ages with the range. Disability weights sourced:
#'  \href{https://ghdx.healthdata.org/record/ihme-data/gbd-2017-disability-weights}{here}.
#' This is an approximation of YLD estimation from the GBD study; disability due to comorbid conditions
#' such as motor impairment and aneamia are excluded.
#'
#' Note: Default parameter values are for _Plasmodium falciparum_
#'
#' @param x Input data.frame
#' @param time_divisor Aggregation level. Default = 1 (no aggregation).
#' Setting to 365 would allow annual aggregation, 30 monthly, 7 weekly etc.
#' @param baseline_t A baseline time to add to the time output
#' @param age_divisor To transform the age units. For example setting 365 will return age units in years for age-disaggregated outputs
#' @param scaler Scaler for severe cases to deaths. The
#' original fitted scaler for P. falciparum is 0.215. See
#' \href{https://www.thelancet.com/journals/laninf/article/PIIS1473-3099(15)00423-5/fulltext}{Griffin et al (2016)}.
#' @param treatment_scaler The impact of treatment coverage on progression to severe disease and death.
#' The probability of being hospitalised for untreated cases compared to treated cases is difficult to study and is not known, but can 
#' be estimated from available data with several assumptions. Our estimate is derived from data used in a meta-analysis on the impact 
#' of delayed treatment of uncomplicated malaria on progression to severe malaria 
#' \href{https://journals.plos.org/plosmedicine/article?id=10.1371/journal.pmed.1003359}{(Mousa et al, 2020, Plos Medicine)}, which estimated 
#' the relative risk ratio for treatment delays of 1-2, 2-3 and >3 days compared to treatment within 1 day. Here, we assume that the 
#' risk of hospitalisation with severe malaria among untreated cases would be double the risk in this last “delay category” of >3 days 
#' (2 * RR 1.186), giving a relative risk of any severe malaria of 0.578 for treatment within 1 day compared to no treatment (i.e. a treatment
#' scaler of 0.42 for the proportion of severe malaria cases that are prevented by treatment of uncomplicated malaria). However, in sensitivity
#' analysis a range of 0.281-0.843 should be considered for the treatment scaler, which was derived by assuming the risk among untreated cases
#' would be the same as the risk with a treatment delay of >3 days (lower estimate) or 3 times the risk as those with a treatment delay of
#' >3 days (upper estimate).
#' @param aggregate_age Aggregate output over age groups
#' @param mild_disability_weight disability weight for mild malaria. Assigned to clinical cases in those over 5 years old
#' @param moderate_disability_weight disability weight for moderate malaria. Assigned to clinical cases in those under 5 years old
#' @param severe_disability_weight disability weight for severe malaria. Assigned to all severe cases
#' @param clinical_episode_length average length of an episode of clinical malaria
#' @param severe_episode_length average length of an episode of severe malaria
#'
#' @export
get_rates <- function(x,
                      time_divisor,
                      baseline_t,
                      age_divisor,
                      scaler = 0.215,
                      treatment_scaler = 0.42,
                      aggregate_age = FALSE,
                      mild_disability_weight = 0.006,
                      moderate_disability_weight = 0.051,
                      severe_disability_weight = 0.133,
                      clinical_episode_length = 0.01375 * 365,
                      severe_episode_length = 0.04795 * 365){
  cols <- colnames(x)
  if(!"timestep" %in% cols){
    stop("required column `timestep` missing")
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
  # Treatment coverage checking
  if(!"ft" %in% cols){
    message("No treatment coverage variable (ft) in x, assumming = 0")
    x$ft = 0
  }
  x$ft[is.na(x$ft)] <- 0


  clinical_cols <- colnames(x)[grepl("inc_clinical", colnames(x)) & !grepl("p_", colnames(x))]
  severe_cols <- colnames(x)[grepl("inc_sev", colnames(x)) & !grepl("p_", colnames(x))]
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
      treatment_scaler = treatment_scaler
    ) |>
    mortality_rate(
      scaler = scaler
    ) |>
    dalys(
      age_divisor = age_divisor
    ) |>
    dplyr::select(c("t", "age_lower", "age_upper", "clinical", "severe", "mortality", "yld_pp", "yll_pp", "dalys_pp", "n", "prop_n"))

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
#' @param clinical_cols Clinical incidence column names
#' @param severe_cols Severe incidence column names
#' @inheritParams get_rates
rates_format <- function(x, clinical_cols, severe_cols, age_divisor){
    x |>
      dplyr::rename_with( ~ gsub("n_inc_", "", .x, fixed = TRUE), .cols = dplyr::all_of(clinical_cols)) |>
      dplyr::rename_with( ~ gsub("n_inc_", "", .x, fixed = TRUE), .cols = dplyr::all_of(severe_cols)) |>
      tidyr::pivot_longer(
        cols = -c("t", "ft")
      ) |>
    tidyr::separate_wider_delim("name", "_", names = c("name", "age_lower", "age_upper")) |>
    tidyr::pivot_wider(id_cols = c("t", "ft", "age_lower", "age_upper"), names_from = "name", values_from = "value") |>
    dplyr::mutate(
      clinical = .data$clinical / .data$n,
      severe = .data$severe / .data$n,
      age_lower = as.numeric(.data$age_lower) / age_divisor,
      age_upper = as.numeric(.data$age_upper) / age_divisor,
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
      yld_pp = stats::weighted.mean(.data$yld_pp, .data$prop_n),
      yll_pp = stats::weighted.mean(.data$yll_pp, .data$prop_n),
      dalys_pp = stats::weighted.mean(.data$dalys_pp, .data$prop_n),
      n = sum(.data$n),
      .by = "t"
    )
  return(x)
}

