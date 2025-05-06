test_that("dalys works",{
  x <- data.frame(
    age_lower = 0,
    age_upper = 1,
    clinical = 1,
    severe = 1,
    mortality = 1
  )

  out <- dalys(x, ages_as_years = TRUE, life_expectancy = data.frame(age = 0, life_expectancy = 100))

  expect_equal(out$case_disability_weight, 0.051)
  expect_equal(out$yld, 1 * 0.133 * 0.04795 + 1 * 0.051 * 0.01375)
  expect_equal(out$yll, 1 * 100)
  expect_equal(out$dalys, 1 * 0.133 * 0.04795 + 1 * 0.051 * 0.01375 + 100)
})
