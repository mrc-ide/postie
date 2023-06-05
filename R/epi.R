#' Add mortality rate
#'
#' @param x output
#' @param scaler Scaler for severe cases to deaths. The
#' original fitted scaler for P. falciparum is 0.215. See
#' \href{https://www.thelancet.com/journals/laninf/article/PIIS1473-3099(15)00423-5/fulltext}{Griffin et al (2016)}.
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
#' @param x Input data.frame
#' @param treatment_scaler The impact of treatment coverage on progression to severe disease and death.
#' A highly uncertain parameter, the analysis by
#'  \href{https://www.thelancet.com/journals/laninf/article/PIIS1473-3099(15)00423-5/fulltext}{Griffin et al (2016)}
#' sampled from a uniform diastribution (0, 1).
#' @param baseline_treatment The proportion of uncomplicated malaria cases that are effectively treated historically.
treatment_scaling <- function(x, treatment_scaler, baseline_treatment = 0){
  if(treatment_scaler > 1 || treatment_scaler < 0){
    stop("treatment_scaler must be between 0 and 1")
  }
  if(baseline_treatment > 1 || baseline_treatment < 0){
    stop("treatment_scaler must be between 0 and 1")
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
#' Africa profile for 2022. To estimate the expected age in a given raneg we assume
#' exponentially distributed ages with the range.
#'
#' Disability weights sourced \href{https://ghdx.healthdata.org/record/ihme-data/gbd-2017-disability-weights}{here}
#'
#' This is an approximation of YLD estimation from the GBD study; disability due to comorbid conditions
#' such as motor impairment and anemia are excluded.
#'
#' @param x Input data.frame
#' @inheritParams get_rates
#' @param mild_disability_weight disability weight for mild malaria. Assigned to clinical cases in those over 5 years old
#' @param moderate_disability_weight disability weight for moderate malaria. Assigned to clinical cases in those under 5 years old
#' @param severe_disability_weight disability weight for severe malaria. Assigned to all severe cases
#' @param clinical_episode_length average length of an episode of clinical malaria
#' @param severe_episode_length average length of an episode of severe malaria
dalys <- function(
    x,
    age_divisor,
    mild_disability_weight = 0.006,
    moderate_disability_weight = 0.051,
    severe_disability_weight = 0.133,
    clinical_episode_length = 0.01375,
    severe_episode_length = 0.04795
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
