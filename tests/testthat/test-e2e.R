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
    s
  )
  expect_type(rates, "list")
  expect_equal(names(rates), c("year", "month", "week", "day", "time",
                               "age_lower", "age_upper",
                               "clinical", "severe", "mortality",
                               "yld", "yll", "dalys",
                               "person_days"))

  prev <- get_prevalence(
    s
  )
  expect_type(prev, "list")
  expect_equal(names(prev), c("year", "month", "week", "day", "time",
                              "prevalence_2_10"))
})
