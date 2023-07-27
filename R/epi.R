#' Add mortality rate
#'
#' @inheritParams get_rates
#'
#' @return output with mortality rates
#' @export
mortality_rate <- function(x, scaler){
  if(scaler > 1 || scaler < 0){
    stop("scaler must be between 0 and 1")
  }

  x <- x |>
    dplyr::mutate(mortality = scaler * .data$severe)
  return(x)
}

#' Adjust severe (and downstream mortality) rates as a result of treatment coverage.
#'
#' @inheritParams get_rates
#' @param baseline_treatment The proportion of uncomplicated malaria cases that are effectively treated historically.
#' 
treatment_scaling <- function(x, treatment_scaler, baseline_treatment = 0){
  if(treatment_scaler > 1 || treatment_scaler < 0){
    stop("treatment_scaler must be between 0 and 1")
  }
  if(baseline_treatment > 1 || baseline_treatment < 0){
    stop("baseline treatment must be between 0 and 1")
  }
  ts <- (1 - treatment_scaler)
  x <- x |>
    dplyr::mutate(severe = .data$severe * ((ts * .data$ft + (1 - .data$ft)) / (ts * baseline_treatment + (1 - baseline_treatment)))) |>
    dplyr::select(-"ft")
  return(x)
}

#' Disability adjusted life years
#'
#' Calculate Years Lived with Disability (YLDs) and Disability-Adjusted Life-Years
#' based on disability weights from the Global Burden of Disease study. To estimate
#' YLL we assume the average life expectancy for a person aged x years taken from the UN WPP
#' Africa profile for 2022. To estimate the expected age in a given range we assume
#' exponentially distributed ages with the range. 
#' YLLs are calculated by multiplying deaths by the number of years an individual was expected to live past their year of death.
#'
#' Disability weights sourced \href{https://ghdx.healthdata.org/record/ihme-data/gbd-2017-disability-weights}{here}
#'
#' Duration of disease was sourced from \href{https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5321383/}{Winskill et al 2017 (Supplementary Table 8)}
#' 
#' This is an approximation of YLD estimation from the GBD study; disability due to comorbid conditions
#' such as motor impairment and anemia are excluded.
#'
#' Note: weights and lengths are currently for Plasmodium falciparum.
#'
#' @inheritParams get_rates
dalys <- function(
    x,
    age_divisor,
    mild_disability_weight = 0.006,
    moderate_disability_weight = 0.051,
    severe_disability_weight = 0.133,
    clinical_episode_length = 0.01375 * 365,
    severe_episode_length = 0.04795 * 365
)
{
  x |>
    dplyr::mutate(
      e_age = round(
        expected_age(
          lower_age = (.data$age_lower * age_divisor) / 365, # Note this is always in years for DALYs
          upper_age = (.data$age_upper * age_divisor) / 365, # Note this is always in years for DALYs
          average_age = life_expectancy_africa[life_expectancy_africa$age == 0, "life_expectancy"]
        )
      )
    ) |>
    dplyr::left_join(
      life_expectancy_africa, by = c("e_age" = "age")
    ) |>
    dplyr::mutate(
      case_disability_weight = ifelse(.data$e_age <= 5, moderate_disability_weight, mild_disability_weight),
      yld_pp = .data$severe * severe_disability_weight * severe_episode_length +
        .data$clinical * .data$case_disability_weight * clinical_episode_length,
      yll_pp = .data$mortality * .data$life_expectancy,
      dalys_pp = .data$yld_pp + .data$yll_pp
    )
  }
