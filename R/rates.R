#' Extract basic rates from malariasimulation model output
#'
#' Summarises clinical incidence `clinical`, severe incidence `severe` and mortality `mortality`
#' rates as well as per-capita years lived with disability `yld`, years of life
#' lost `yll` and daly `dalys`. All rates are expressed per person per day.
#'
#' For DALYs: disability weights are from the Global Burden of Disease study. To estimate
#' YLL we assume the average life expectancy for a person aged x years taken from the UN WPP
#' sub-Saharan-Africa profile. To estimate the expected age in a given range we assume
#' exponentially distributed ages with the range. Disability weights sourced:
#'  \href{https://ghdx.healthdata.org/record/ihme-data/gbd-2017-disability-weights}{here}.
#' This is an approximation of YLD estimation from the GBD study; disability due to comorbid conditions
#' such as motor impairment and aneamia are excluded.
#'
#' Note: Default parameter values are for _Plasmodium falciparum_
#'
#' @param x Input data.frame
#' @param baseline_year Baseline year (assumes simulation starts on the first day of the year)
#' @param ages_as_years Convert ages to be in units of years
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
#' @param mild_disability_weight disability weight for mild malaria. Assigned to clinical cases in those over 5 years old
#' @param moderate_disability_weight disability weight for moderate malaria. Assigned to clinical cases in those under 5 years old
#' @param severe_disability_weight disability weight for severe malaria. Assigned to all severe cases
#' @param clinical_episode_length average length of an episode of clinical malaria
#' @param severe_episode_length average length of an episode of severe malaria
#' @param life_expectancy data.frame of expected years left to live. See example in data for format
#' @param infer_ft If ft not found in model output (usually if ft = 0), assume ft = 0. If FALSE an error will be thrown
#'
#' @export
get_rates <- function(x,
                      baseline_year = 2000, ages_as_years = TRUE,
                      scaler = 0.215,
                      treatment_scaler = 0.42,
                      mild_disability_weight = 0.006,
                      moderate_disability_weight = 0.051,
                      severe_disability_weight = 0.133,
                      clinical_episode_length = 0.01375,
                      severe_episode_length = 0.04795,
                      life_expectancy = life_expectancy_africa,
                      infer_ft = TRUE
){
  cols <- colnames(x)
  if(!"timestep" %in% cols){
    stop("required column `timestep` missing")
  }
  if(!"ft" %in% cols){
    if(infer_ft){
      warning("required column `ft` not found, assumming ft = 0")
      x$ft <- 0
    } else {
      stop("required column `ft` missing")
    }
  } else {
    x$ft[is.na(x$ft)] <- 0
  }
  if(sum(grepl("n_inc_clinical", cols)) == 0){
    stop("required columns `n_inc_clinical_...` missing")
  }
  if(sum(grepl("n_inc_severe", cols)) == 0){
    stop("required columns `n_inc_severe_...` missing")
  }
  clinical_cols <- colnames(x)[grepl("inc_clinical", colnames(x)) & !grepl("p_", colnames(x))]
  severe_cols <- colnames(x)[grepl("inc_sev", colnames(x)) & !grepl("p_", colnames(x))]
  denominator_cols <- stringr::str_replace(clinical_cols, "_inc_clinical", "")

  if(length(clinical_cols) != length(severe_cols)){
    stop("The same age_min and age_max must be provided for clinical and severe incidence")
  }

  rates <- x |>
    dplyr::select(dplyr::all_of(c("timestep", "ft", clinical_cols, severe_cols, denominator_cols))) |>
    rates_format(
      clinical_cols = clinical_cols,
      severe_cols = severe_cols,
      ages_as_years = ages_as_years
    ) |>
    treatment_scaling(
      treatment_scaler = treatment_scaler,
      baseline_treatment = 0.1
    ) |>
    mortality_rate(
      scaler = scaler
    ) |>
    format_time(
      baseline_year = baseline_year
    )  |>
    dalys(
      ages_as_years = ages_as_years,
      life_expectancy = life_expectancy,
      mild_disability_weight = mild_disability_weight,
      moderate_disability_weight = moderate_disability_weight,
      severe_disability_weight = severe_disability_weight,
      clinical_episode_length = clinical_episode_length,
      severe_episode_length = severe_episode_length
    ) |>
    dplyr::select(c(
      "year", "month", "week", "day", "time",
      "age_lower", "age_upper",
      "clinical", "severe", "mortality",
      "yld", "yll", "dalys",
      "person_days"))

  return(rates)
}

#' Format rates
#'
#' Create rates from counts and specifies age output units
#'
#' @inheritParams get_rates
#' @param clinical_cols Clinical incidence column names
#' @param severe_cols Severe incidence column names
rates_format <- function(x, clinical_cols, severe_cols, ages_as_years){
  age_divisor = ifelse(ages_as_years, 365, 1)

  x |>
    dplyr::rename_with( ~ gsub("n_inc_", "", .x, fixed = TRUE), .cols = dplyr::all_of(clinical_cols)) |>
    dplyr::rename_with( ~ gsub("n_inc_", "", .x, fixed = TRUE), .cols = dplyr::all_of(severe_cols)) |>
    tidyr::pivot_longer(
      cols = -c("timestep", "ft")
    ) |>
    tidyr::separate_wider_delim("name", "_", names = c("name", "age_lower", "age_upper")) |>
    tidyr::pivot_wider(id_cols = c("timestep", "ft", "age_lower", "age_upper"), names_from = "name", values_from = "value") |>
    dplyr::mutate(
      clinical = .data$clinical / .data$n,
      severe = .data$severe / .data$n,
      age_lower = as.numeric(.data$age_lower) / age_divisor,
      age_upper = as.numeric(.data$age_upper) / age_divisor,
      person_days = round(.data$n)
    )
}

#' Aggregate rates
#'
#' @param rates formatted rates
#' @param ... Aggregation columns
rates_aggregate <- function(rates, ...){
  rates |>
    dplyr::summarise(
      clinical = stats::weighted.mean(.data$clinical, .data$person_days),
      severe = stats::weighted.mean(.data$severe, .data$person_days),
      mortality = stats::weighted.mean(.data$mortality, .data$person_days),
      yll = stats::weighted.mean(.data$yll, .data$person_days),
      yld = stats::weighted.mean(.data$yld, .data$person_days),
      dalys = stats::weighted.mean(.data$dalys, .data$person_days),
      person_days = sum(.data$person_days),
      time = mean(.data$time),
      .by = dplyr::all_of(...)
    )
}
