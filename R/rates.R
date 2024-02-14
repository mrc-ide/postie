#' Extract basic rates from model output
#'
#' @param x Input data.frame
#' @param baseline year Starting year, assuming the model runs begins at the start of the year
#' @param ages_as_years Converts ages from days to years
#' @param scaler Scaler for severe cases to deaths. The
#' original fitted scaler for P. falciparum is 0.215. See
#' \href{https://www.thelancet.com/journals/laninf/article/PIIS1473-3099(15)00423-5/fulltext}{Griffin et al (2016)}.
#' @param treatment_scaler The impact of treatment coverage on progression to severe disease and death.
#' A highly uncertain parameter, the analysis by
#'  \href{https://www.thelancet.com/journals/laninf/article/PIIS1473-3099(15)00423-5/fulltext}{Griffin et al (2016)}
#' sampled from a uniform diastribution (0, 1).
#'
#' @export
get_rates <- function(x,
                      baseline_year = 2000, ages_as_years = TRUE,
                      scaler = 0.215, treatment_scaler = 0.5,
                      life_expectancy = life_expectancy_africa
){
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
      life_expectancy = life_expectancy
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
#' @param x Input data.frame
#' @param clinical_cols Clinical incidence column names
#' @param severe_cols Severe incidence column names
#' @param age_divisor Aggregation level. For example setting to 365 will return
#' age units in years
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
      age_lower = round(as.numeric(.data$age_lower) / age_divisor),
      age_upper = round(as.numeric(.data$age_upper) / age_divisor),
      person_days = round(.data$n)
    )
}

rates_aggregate <- function(rates, ...){
  rates |>
    summarise(
      clinical = weighted.mean(clinical, person_days),
      severe = weighted.mean(severe, person_days),
      mortality = weighted.mean(mortality, person_days),
      yll = weighted.mean(yll, person_days),
      yld = weighted.mean(yld, person_days),
      dalys = weighted.mean(dalys, person_days),
      person_days = sum(person_days),
      time = mean(time),
      .by = dplyr::all_of(...)
    )
}
