test_that("time_transform works", {
  mock_data <- data.frame(
    timestep = 1:(365 * 5)
  )

  expected_output <- data.frame(
    t = 1:(365 * 5)
  )
  expect_identical(time_transform(mock_data), expected_output)

  expected_year <- data.frame(
    t = rep(1:5, each = 365)
  )
  expect_identical(
    time_transform(mock_data, time_divisor = 365),
    expected_year
  )

  expected_baseline<- data.frame(
    t = 2000L + rep(1:5, each = 365)
  )
  expect_identical(
    time_transform(mock_data, time_divisor = 365, baseline_t = 2000),
    expected_baseline
  )

  expect_warning(
    time_transform(mock_data, time_divisor = 7),
    "Number of timesteps not divisible exactly by level, group may be unequal"
  )
})

test_that("check drop burnin works", {
  # create test data
  test_data <- data.frame(
    timestep = 1:10,
    value = 11
  )
  for(i in 1:9){
    actual_output <- drop_burnin(test_data, i)
    expected_data <- test_data[(i + 1):10,]
    expected_data$timestep <- expected_data$timestep - i
    expect_identical(expected_data$timestep, actual_output$timestep)
    expect_identical(expected_data$value, actual_output$value)
  }

  expect_error(drop_burnin(test_data, 10), "burn in period must be < the maximum timestep")
  expect_error(drop_burnin(test_data, 11), "burn in period must be < the maximum timestep")
  expect_error(drop_burnin(test_data, -1), "burn in period must be positive")
})
