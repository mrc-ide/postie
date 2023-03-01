test_that("function throws error when 'timestep' is missing", {
  x <- data.frame(ft = c(1, 2, 3))
  expect_error(rates_column_check(x), "required column `timestep` missing")
})

test_that("function throws error when 'ft' is missing", {
  x <- data.frame(timestep = c(1, 2, 3))
  expect_error(rates_column_check(x), "required column `ft` missing")
})

test_that("function throws error when 'n_inc_clinical_...' are missing", {
  x <- data.frame(
    timestep = c(1, 2, 3),
    ft = c(1, 2, 3))

  expect_error(rates_column_check(x), "required columns `n_inc_clinical_...` missing")
})

test_that("function throws error when 'n_inc_severe_...' are missing", {
  x <- data.frame(
    timestep = c(1, 2, 3),
    ft = c(1, 2, 3),
    n_inc_clinical_0_9 = c(1, 2, 3)
  )
  expect_error(rates_column_check(x), "required columns `n_inc_severe_...` missing")
})

test_that("function throws error when 'n_age_...' are missing", {
  x <- data.frame(
    timestep = c(1, 2, 3),
    ft = c(1, 2, 3),
    n_inc_clinical_0_9 = c(1, 2, 3),
    n_inc_severe_0_9 = c(1, 2, 3)
  )
  expect_error(rates_column_check(x), "required columns `n_age_...` missing")
})

test_that("function throws error when age ranges for 'n_inc_clinical_...', 'n_inc_severe_...', and 'n_age_...' are not the same", {
  x <- data.frame(
    timestep = c(1, 2, 3),
    ft = c(1, 2, 3),
    n_inc_clinical_0_9 = c(1, 2, 3),
    n_inc_severe_0_10 = c(1, 2, 3),
    n_age_0_9 = c(1, 2, 3)
  )
  expect_error(rates_column_check(x), "Age ranges for `n_inc_clinical_...` and `n_inc_severe_...` and `n_age_...` outputs must be the same")

  x <- data.frame(
    timestep = c(1, 2, 3),
    ft = c(1, 2, 3),
    n_inc_clinical_0_9 = c(1, 2, 3),
    n_inc_severe_0_9 = c(1, 2, 3),
    n_age_0_10 = c(1, 2, 3)
  )
  expect_error(rates_column_check(x), "Age ranges for `n_inc_clinical_...` and `n_inc_severe_...` and `n_age_...` outputs must be the same")
})


test_that("rates_transform returns an expected data frame", {
  # Create a mock dataset for testing
  mock_data <- data.frame(
    timestep = c(1, 2, 3, 4),
    ft = c(0.1, 0.1, 0.2, 0.2),
    n_age_0_4 = c(10, 10, 11, 11),
    n_age_5_9 = c(20, 20, 21, 21),
    n_inc_clinical_0_4 = c(1, 0, 1, 4),
    n_inc_clinical_5_9 = c(2, 2, 0, 1),
    n_inc_severe_0_4 = c(1, 1, 2, 1),
    n_inc_severe_5_9 = c(2, 1, 1, 1)
  )

  transformed_data <- rates_transform(mock_data)
  expect_equal(ncol(transformed_data), 7)
  expect_equal(nrow(transformed_data), 8)
  expected_cols <- c("timestep", "age_lower", "age_upper", "ft", "age", "clinical", "severe")
  expect_equal(colnames(transformed_data), expected_cols)

  expected_timestep <- rep(mock_data$timestep, each = 2)
  expect_equal(transformed_data$timestep, expected_timestep)
  expected_age_lower <- rep(c(0, 5), 4)
  expect_equal(transformed_data$age_lower, expected_age_lower)
  expected_age_upper <- rep(c(4, 9), 4)
  expect_equal(transformed_data$age_upper, expected_age_upper)
  expected_ft <- rep(mock_data$ft, each = 2)
  expect_equal(transformed_data$ft, expected_ft)
  expected_age <- c(10, 20, 10, 20, 11, 21, 11, 21)
  expect_equal(transformed_data$age, expected_age)
  expected_clinical <- c(1, 2, 0, 2, 1, 0, 4, 1)
  expect_equal(transformed_data$clinical, expected_clinical)
  expected_severe <- c(1, 2, 1, 1, 2, 1, 1, 1)
  expect_equal(transformed_data$severe, expected_severe)

})

test_that("rates time aggregation works", {
  mock_data <- data.frame(
    t = c(1, 1, 2, 2),
    age_lower = c(1, 1, 1, 1),
    age_upper = c(5, 5, 5, 5),
    clinical = 1:4,
    severe = 5:8,
    age = 1:4,
    ft = 5:8
  )
  output <- rates_time_aggregate(mock_data)
  expect_equal(output$t, unique(mock_data$t))
  expect_equal(output$clinical, c(sum(1:2), sum(3:4)))
  expect_equal(output$severe, c(sum(5:6), sum(7:8)))
  expect_equal(output$age, c(mean(1:2), mean(3:4)))
  expect_equal(output$ft, c(mean(5:6), mean(7:8)))
})

test_that("rates format works", {
  mock_data <- data.frame(
    t = c(1, 1, 2, 2),
    age_lower = c(1, 1, 1, 1),
    age_upper = c(5, 5, 5, 5),
    clinical = 1:4,
    severe = 5:8,
    age = 1:4,
    ft = 5:8
  )
  output <- rates_format(mock_data, age_divisor = 1)
  expect_equal(output$t, mock_data$t)
  expect_equal(output$age_lower, mock_data$age_lower)
  expect_equal(output$age_upper, mock_data$age_upper)
  expect_equal(output$clinical, mock_data$clinical / mock_data$age)
  expect_equal(output$severe, mock_data$severe / mock_data$age)
  expect_equal(output$prop_age, mock_data$age / c(3, 3, 7, 7))
  expect_equal(output$ft, mock_data$ft)

  expect_error(
    rates_format(mock_data, age_divisor = 0.5),
    "age_divisor must be > 1"
  )
})

test_that("rates format works", {
  mock_data <- data.frame(
    t = c(1, 1, 2, 2),
    clinical = c(0.1, 0.2, 0.3, 0.4),
    severe = c(0.01, 0.02, 0.03, 0.04),
    mortality = c(0.001, 0.002, 0.003, 0.004),
    prop_age = c(0.25, 0.75, 0.25, 0.75)
  )
  output <- rates_age_aggregate(mock_data)
  expect_equal(output$t, c(1, 2))
  expect_equal(
    output$clinical,
    c(
      weighted.mean(c(0.1,0.2), c(0.25,0.75)),
      weighted.mean(c(0.3, 0.4), c(0.25, 0.75))
    )
  )
  expect_equal(
    output$severe,
    c(
      weighted.mean(c(0.01,0.02), c(0.25,0.75)),
      weighted.mean(c(0.03, 0.04), c(0.25, 0.75))
    )
  )
  expect_equal(
    output$mortality,
    c(
      weighted.mean(c(0.001,0.002), c(0.25,0.75)),
      weighted.mean(c(0.003, 0.004), c(0.25, 0.75))
    )
  )
})
