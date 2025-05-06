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

  expect_error(
    get_rates(x = data.frame(timestep = 1, ft = 0.5, ft_sev = 0.7, n_inc_clinical_0_100 = 1)),
    "required columns `n_inc_severe_...` missing"
  )
})

