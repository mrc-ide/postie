test_that("get rates work", {
  set.seed(1)
  mock_data <- data.frame(
    timestep = c(1, 2),
    n_inc_clinical_0_10 = 1:2,
    n_inc_severe_0_10 = 1:2 / 10,
    n_0_10 = 1:2 * 10,
    ft = 0.2
  )

  out <- get_rates(
    x = mock_data,
    time_divisor = 1,
    baseline_t = 0,
    age_divisor = 1,
    scaler = 1,
    treatment_scaler = 0,
    aggregate_age = FALSE
  )
  expected_out <- data.frame(
    t = 1:2,
    age_lower = 0,
    age_upper = 10,
    clinical = 1:2 / (1:2 * 10),
    severe = (1:2 / 10) / (1:2 * 10),
    mortality = (1:2 / 10) / (1:2 * 10),
    yld_pp = 0.0001338985*365,
    yll_pp = 0.617,
    dalys_pp = 0.0001338985*365 + 0.617,
    n = c(10, 20),
    prop_n = 1
  )

  for(i in 1:ncol(mock_data)){
    expect_identical(out[[i]], expected_out[[i]])
  }

  # Agg aggregated
  out2 <- get_rates(
    x = mock_data,
    time_divisor = 1,
    baseline_t = 0,
    age_divisor = 1,
    scaler = 1,
    treatment_scaler = 0,
    aggregate_age = TRUE
  )
  expected_out2 <- data.frame(
    t = 1:2,
    clinical = 1:2 / (1:2 * 10),
    severe = (1:2 / 10) / (1:2 * 10),
    mortality = (1:2 / 10) / (1:2 * 10),
    yld_pp = 0.0001338985*365,
    yll_pp = 0.617,
    dalys_pp = 0.0001338985*365 + 0.617,
    n = c(10, 20)
  )

  for(i in 1:ncol(mock_data)){
    expect_identical(out2[[i]], expected_out2[[i]])
  }
})


test_that("get rates input checks work", {
  expect_error(
    get_rates(x = data.frame()),
    "required column `timestep` missing"
  )

  expect_error(
    get_rates(x = data.frame(timestep = 1, ft = 0.5)),
    "required columns `n_inc_clinical_...` missing"
  )

  expect_error(
    get_rates(x = data.frame(timestep = 1, ft = 0.5, n_inc_clinical_0_100 = 1)),
    "required columns `n_inc_severe_...` missing"
  )

  expect_message(
    get_rates(x = data.frame(
      timestep = 1,
      n_inc_clinical_0_100 = 1,
      n_inc_severe_0_100 = 0.1,
      n_0_100 = 1
    ),
    age_divisor = 1,
    time_divisor = 1,
    baseline_t = 0),
    "No treatment coverage variable (ft) in x, assumming = 0",
    fixed = TRUE
  )

  expect_error(
    get_rates(x = data.frame(timestep = 1, ft = 0.5, n_inc_clinical_0_100 = 1, n_inc_severe_0_100 = 0.1),
              scaler = 1, treatment_scaler = 1, baseline_t = 1, time_divisor = 1, age_divisor = 0.1),
    "age_divisor must be >= 1"
  )
})

test_that("rates time aggregation works", {
  mock_data <- data.frame(
    t = c(1, 1, 2, 2),
    age_lower = c(1, 1, 1, 1),
    age_upper = c(5, 5, 5, 5),
    clinical = 1:4,
    severe = 5:8,
    n = 1:4,
    ft = 5:8
  )
  output <- rates_time_aggregate(mock_data, "clinical", "severe", "n")
  expect_equal(output$t, unique(mock_data$t))
  expect_equal(output$clinical, c(sum(1:2), sum(3:4)))
  expect_equal(output$severe, c(sum(5:6), sum(7:8)))
  expect_equal(output$n, c(mean(1:2), mean(3:4)))
  expect_equal(output$ft, c(mean(5:6), mean(7:8)))
})

test_that("rates format works", {
  mock_data <- data.frame(
    t = c(1, 1, 2, 2),
    n_inc_clinical_0_10 = 1:4,
    n_inc_severe_0_10 = 1:4 / 10,
    n_0_10 = 1:4 * 10,
    ft = 5:8
  )
  clinical_cols <- c("n_inc_clinical_0_10")
  severe_cols <- c("n_inc_severe_0_10")
  output <- rates_format(mock_data, clinical_cols = clinical_cols, severe_cols = severe_cols, age_divisor = 1)
  expect_equal(output$t, mock_data$t)
  expect_equal(output$age_lower, rep(0, 4))
  expect_equal(output$age_upper, rep(10, 4))
  expect_equal(output$clinical, mock_data$n_inc_clinical_0_10 / mock_data$n_0_10)
  expect_equal(output$severe, mock_data$n_inc_severe_0_10 / mock_data$n_0_10)
  expect_equal(output$prop_n, c(c(10, 20) / 30, c(30, 40) / 70))
  expect_equal(output$ft, mock_data$ft)
})

test_that("rates age aggregation works", {
  mock_data <- data.frame(
    t = c(1, 1, 2, 2),
    clinical = c(0.1, 0.2, 0.3, 0.4),
    severe = c(0.01, 0.02, 0.03, 0.04),
    mortality = c(0.001, 0.002, 0.003, 0.004),
    yld_pp = 0.1,
    yll_pp = 0.2,
    dalys_pp = 0.3,
    n = c(25, 75, 25, 75),
    prop_n = c(0.25, 0.75, 0.25, 0.75)
  )
  output <- rates_age_aggregate(mock_data)
  expect_equal(output$t, c(1, 2))
  expect_equal(
    output$clinical,
    c(
      weighted.mean(c(0.1, 0.2), c(0.25,0.75)),
      weighted.mean(c(0.3, 0.4), c(0.25, 0.75))
    )
  )
  expect_equal(
    output$severe,
    c(
      weighted.mean(c(0.01, 0.02), c(0.25,0.75)),
      weighted.mean(c(0.03, 0.04), c(0.25, 0.75))
    )
  )
  expect_equal(
    output$mortality,
    c(
      weighted.mean(c(0.001, 0.002), c(0.25,0.75)),
      weighted.mean(c(0.003, 0.004), c(0.25, 0.75))
    )
  )
  expect_equal(
    output$yld_pp,
    c(
      weighted.mean(c(0.1, 0.1), c(0.25,0.75)),
      weighted.mean(c(0.1, 0.1), c(0.25, 0.75))
    )
  )
  expect_equal(
    output$yll_pp,
    c(
      weighted.mean(c(0.2, 0.2), c(0.25,0.75)),
      weighted.mean(c(0.2, 0.2), c(0.25, 0.75))
    )
  )
  expect_equal(
    output$dalys_pp,
    c(
      weighted.mean(c(0.3, 0.3), c(0.25,0.75)),
      weighted.mean(c(0.3, 0.3), c(0.25, 0.75))
    )
  )
  expect_equal(
    output$n,
    c(100, 100)
  )
})
