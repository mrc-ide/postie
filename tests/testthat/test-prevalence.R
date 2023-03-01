test_that("prevalence_estimate works", {
  mock_data <- data.frame(
    timestep = 1:4,
    n_detect_1_2 = c(0, 1, 2, 3),
    n_1_2 = 10
  )
  output <- prevalence_estimate(mock_data)
  expect_equal(output$n_detect_1_2, mock_data$n_detect_1_2 / 10)
})

test_that("prevalence_time_aggregate works", {
  mock_data <- data.frame(
    t = c(1, 1, 2, 2),
    n_detect_1_2 = c(0, 1, 2, 3)
  )
  output <- prevalence_time_aggregate(mock_data)
  expect_equal(output$n_detect_1_2, c(mean(0:1), mean(2:3)))
})

test_that("prevalence_format works", {
  mock_data <- data.frame(
    t = c(1, 1, 2, 2),
    n_detect_1_2 = c(0, 1, 2, 3)
  )
  output <- prevalence_format(mock_data, age_divisor = 1)
  expect_equal(output$prevalence_1_2, mock_data$n_detect_1_2)
  expect_error(prevalence_format(mock_data, age_divisor = 0), "age_divisor must be > 1")
})
