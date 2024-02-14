test_that("mortality works", {
  mock_data <- data.frame(
    t = 1:5,
    severe = 11:15
  )
  output <- mortality_rate(mock_data, scaler = 0.215)
  expect_equal(output$mortality, 0.215 * mock_data$severe)
    expect_error(
    mortality_rate(mock_data, scaler = -1),
    "scaler must be between 0 and 1"
  )
  expect_error(
    mortality_rate(mock_data, scaler = 2),
    "scaler must be between 0 and 1"
  )
})

test_that("treatment scaling works", {
  mock_data <- data.frame(
    t = 1:5,
    severe = 11:15,
    ft = c(0, 0.1, 0.2, 0.3, 0.4)
  )
  output <- treatment_scaling(mock_data, treatment_scaler = 0)
  expect_equal(output$severe, mock_data$severe)

  output_ts <- treatment_scaling(mock_data, treatment_scaler = 0.5, baseline_treatment = 0.4)
  expect_equal(output_ts$severe, mock_data$severe * ((0.5 * mock_data$ft + (1 - mock_data$ft)) / (0.5 * 0.4 + (1 - 0.4))))

  expect_error(
    treatment_scaling(mock_data, treatment_scaler = -1),
    "treatment_scaler must be between 0 and 1"
  )
  expect_error(
    treatment_scaling(mock_data, treatment_scaler = 2),
    "treatment_scaler must be between 0 and 1"
  )
})

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
