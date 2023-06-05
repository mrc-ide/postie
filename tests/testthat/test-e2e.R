rlang::check_installed("malariasimulation")

test_that("Functions work on malariasimulation output", {
  p <- malariasimulation::get_parameters() |>
    malariasimulation::set_drugs(list(malariasimulation::AL_params)) |>
    malariasimulation::set_clinical_treatment(1, 1, 0.5)

  year <- 365
  min_ages <- year * 0
  max_ages <- year * 100
  p$clinical_incidence_rendering_min_ages = min_ages
  p$clinical_incidence_rendering_max_ages = max_ages
  p$severe_incidence_rendering_min_ages = min_ages
  p$severe_incidence_rendering_max_ages = max_ages

  s <- malariasimulation::run_simulation(100, p)

  rates <- get_rates(
    s,
    time_divisor = 1,
    baseline_t = 0,
    age_divisor = 1,
    scaler = 0.215,
    treatment_scaler = 0.5,
    baseline_treatment = 0
  )
  expect_type(rates, "list")
  expect_equal(names(rates), c("t", "age_lower", "age_upper",
                               "clinical", "severe", "mortality",
                               "yld_pp", "yll_pp", "dalys_pp",
                               "n", "prop_n"))

  rates <- get_rates(
    s,
    time_divisor = 1,
    baseline_t = 0,
    age_divisor = 1,
    scaler = 0.215,
    treatment_scaler = 0.5,
    baseline_treatment = 0,
    aggregate_age = TRUE
  )
  expect_type(rates, "list")
  expect_equal(names(rates), c("t",
                               "clinical", "severe", "mortality",
                               "yld_pp", "yll_pp", "dalys_pp",
                               "n"))

  prev <- get_prevalence(
    s,
    time_divisor = 1,
    baseline_t = 0,
    age_divisor = 1
  )
  expect_type(prev, "list")
  expect_equal(names(prev), c("t", "prevalence_730_3650"))
})
