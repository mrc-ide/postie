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
    ages_as_years,
    life_expectancy,
    mild_disability_weight = 0.006,
    moderate_disability_weight = 0.051,
    severe_disability_weight = 0.133,
    clinical_episode_length = 0.01375,
    severe_episode_length = 0.04795
){
  age_divisor <- ifelse(ages_as_years, 1, 365)
  x |>
    dplyr::mutate(
      e_age = round(
        expected_age(
          lower_age = (.data$age_lower * age_divisor) / age_divisor, # Note this is always in years for DALYs
          upper_age = (.data$age_upper * age_divisor) / age_divisor, # Note this is always in years for DALYs
          average_age = life_expectancy[life_expectancy$age == 0, "life_expectancy"]
        )
      )
    ) |>
    dplyr::left_join(
      life_expectancy, by = c("e_age" = "age")
    ) |>
    dplyr::mutate(
      case_disability_weight = ifelse(.data$e_age <= 5, moderate_disability_weight, mild_disability_weight),
      yld_pp = .data$severe * severe_disability_weight * severe_episode_length +
        .data$clinical * .data$case_disability_weight * clinical_episode_length,
      yll_pp = .data$mortality * .data$life_expectancy,
      dalys_pp = .data$yld_pp + .data$yll_pp
    )
}

# Define the function for the integrand
integrand <- function(x, rate){
  x * stats::dexp(x, rate)
}

#' Expected age within range assuming exponentially distributed pop
#'
#' @param lower_age Lower bound of age range (single value)
#' @param upper_age Upper bound of age range (single value)
#' @param average_age Average age of population
ea <- function(lower_age, upper_age, average_age){
  rate <- 1 / average_age
  stats::integrate(integrand, lower_age, upper_age, rate = rate)$value /
    (stats::pexp(upper_age, rate) - stats::pexp(lower_age, rate))
}

#' Expected age within range assuming exponentially distributed pop
#'
#' @param lower_age Lower bound of age range (can be a vector)
#' @param upper_age Upper bound of age range (can be a vector)
#' @param average_age Average age of population
expected_age <- Vectorize(ea, vectorize.args = c("lower_age", "upper_age"))
