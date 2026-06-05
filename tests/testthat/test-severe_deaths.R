# --------------------------- constants ------------------------------------
mH        <- 0.065      # hospital CFR
mC        <- 0.60       # community CFR
nu_target <- 0.215      # 0.065 + 0.60 * 0.2 / 0.8
chi       <- 0.50       # treatment_scaler used in most tests

# --------------------------- mock data ------------------------------------
mock_data <- data.frame(
  t                 = 1:5,
  severe            = 11:15 / 1000,
  ft                = 0.75,            # ensure scaling = 1  (see note below)
  ft_sev            = 0.8           # implementation ignores it
)

# helper: run once for the common scenario
out_default <- severe_incidence_mortality(
  x                 = mock_data,
  treatment_scaler  = chi,          # 0.50
  hosp_sev_cfr      = mH,
  community_sev_cfr = mC
)

test_that("function returns the expected columns", {
  want <- c("severe_hospital", "severe_community", "severe",
            "mortality_hospital", "mortality_community", "mortality")
  expect_true(all(want %in% names(out_default)))
})

test_that("treatment_scaler outside [0,1] throws an error", {
  expect_error(
    severe_incidence_mortality(mock_data, treatment_scaler = 1.5),
    regexp = "between 0 and 1"
  )
})

test_that("row‑wise sums are correct", {
  expect_equal(out_default$severe,
               out_default$severe_hospital + out_default$severe_community,
               tolerance = 1e-12)

  expect_equal(out_default$mortality,
               out_default$mortality_hospital + out_default$mortality_community,
               tolerance = 1e-12)
})

test_that("Original fitted ratio of deaths to hospitalised cases hold when ft_sev = 0.8", {
  ratio <- out_default$mortality / out_default$severe_hospital
  expect_equal(ratio, rep(nu_target, length(ratio)), tolerance = 1e-12)
})

test_that("λ_C / λ_H ratio equals (1-ρ)/ρ for each row", {
  expect_equal(out_default$severe_community / out_default$severe_hospital,
               rep((1 - 0.8) / 0.8, nrow(out_default)),
               tolerance = 1e-12)
})

test_that("severe incidence is normalised to the f0 = 0.1 baseline treatment (Griffin 2016 SI)", {
  f0 <- 0.1
  ts <- 1 - chi
  # With ft_sev = 0.8 the hospitalised severe incidence equals the raw model
  # severe scaled by the treatment adjustment relative to the f0 baseline:
  #   (ts * ft + (1 - ft)) / (ts * f0 + (1 - f0))
  expected_hosp <- mock_data$severe *
    (mock_data$ft * ts + (1 - mock_data$ft)) / (f0 * ts + (1 - f0))
  expect_equal(out_default$severe_hospital, expected_hosp, tolerance = 1e-12)
})
