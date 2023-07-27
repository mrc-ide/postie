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

  output_ts <- treatment_scaling(mock_data, treatment_scaler = 0.43, baseline_treatment = 0.4)
  expect_equal(output_ts$severe, mock_data$severe * (((1-0.43) * mock_data$ft + (1 - mock_data$ft)) / ((1-0.43) * 0.4 + (1 - 0.4))))

  expect_error(
    treatment_scaling(mock_data, treatment_scaler = -1),
    "treatment_scaler must be between 0 and 1"
  )
  expect_error(
    treatment_scaling(mock_data, treatment_scaler = 2),
    "treatment_scaler must be between 0 and 1"
  )
})
