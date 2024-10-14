test_that("time_transform works", {
  mock_data <- data.frame(
    timestep = 1:(365 * 5)
  )

  expected_output <- data.frame(
    time =  2000 + (mock_data$timestep - 1) / 365,
    year = rep(2000:2004, each = 365),
    month = rep(1:12, c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)),
    week = c(rep(1:52, each = 7), 52),
    day = 1:365,
    timestep = 1:(365 * 5)
  )
  expect_equal(format_time(mock_data, baseline_year = 2000), expected_output)

  expected_output2 <- data.frame(
    time =  2010 + (mock_data$timestep - 1) / 365,
    year = rep(2010:2014, each = 365),
    month = rep(1:12, c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)),
    week = c(rep(1:52, each = 7), 52),
    day = 1:365,
    timestep = 1:(365 * 5)
  )
  expect_equal(format_time(mock_data, baseline_year = 2010), expected_output2)
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
