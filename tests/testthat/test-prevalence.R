test_that("prevalence_estimate works", {
  mock_data <- data.frame(
    timestep = 1:4,
    n_detect_lm_1_2 = c(0, 1, 2, 3),
    n_age_1_2 = 10
  )
  output <- prevalence_estimate(mock_data, diagnostic = "lm")
  expect_equal(output$n_detect_lm_1_2, mock_data$n_detect_lm_1_2 / 10)
})

test_that("prevalence_format works", {
  mock_data <- data.frame(
    timestep = c(1, 1, 2, 2),
    n_detect_lm_1_2 = c(0, 1, 2, 3)
  )
  output <- prevalence_format(mock_data, ages_as_years = FALSE, diagnostic = "lm")
  expect_equal(output$lm_prevalence_1_2, mock_data$n_detect_lm_1_2)
})
