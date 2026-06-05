test_that("get rates input checks work", {
  expect_error(
    get_rates(x = data.frame()),
    "required column `timestep` missing"
  )

  expect_error(
    get_rates(x = data.frame(timestep = 1), infer_ft = FALSE),
    "required column `ft` missing"
  )

  expect_error(
    get_rates(x = data.frame(timestep = 1, ft = 0.5), infer_ft_sev = FALSE),
    regexp  = "required column `ft_sev`"
  )

  expect_error(
    get_rates(x = data.frame(timestep = 1, ft = 0.5, ft_sev = 0.8)),
    "required columns `n_inc_clinical_...` missing"
  )
})

test_that("missing severe incidence columns are inferred as NA with a warning", {
  # e.g. P. vivax output has no `n_inc_severe_...` columns
  x <- data.frame(
    timestep = 1,
    ft = 0.5,
    ft_sev = 0.8,
    n_inc_clinical_0_36500 = 10,
    n_age_0_36500 = 1000
  )

  expect_warning(
    rates <- get_rates(x),
    "required columns `n_inc_severe_...` not found"
  )

  # clinical incidence is still produced...
  expect_false(is.na(rates$clinical))
  # ...but all severe, mortality and DALY columns are NA
  na_cols <- c("severe_hospital", "severe_community", "severe",
               "mortality_hospital", "mortality_community", "mortality",
               "yld", "yll", "dalys")
  expect_true(all(is.na(rates[, na_cols])))
})

