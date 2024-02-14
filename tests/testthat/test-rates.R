test_that("get rates input checks work", {
  expect_error(
    get_rates(x = data.frame()),
    "required column `timestep` missing"
  )

  expect_error(
    get_rates(x = data.frame(timestep = 1)),
    "required column `ft` missing"
  )

  expect_error(
    get_rates(x = data.frame(timestep = 1, ft = 0.5)),
    "required columns `n_inc_clinical_...` missing"
  )

  expect_error(
    get_rates(x = data.frame(timestep = 1, ft = 0.5, n_inc_clinical_0_100 = 1)),
    "required columns `n_inc_severe_...` missing"
  )
})

