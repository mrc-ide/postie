test_that("rates_aggregate computes person_days-weighted means by group", {
  rates <- data.frame(
    age_group = c("a", "a", "b", "b"),
    clinical = c(1, 3, 2, 2),
    severe = c(0.1, 0.3, 0.2, 0.2),
    mortality = c(0.01, 0.03, 0.02, 0.02),
    yll = c(1, 1, 2, 2),
    yld = c(0.5, 0.5, 1, 1),
    dalys = c(1.5, 1.5, 3, 3),
    person_days = c(10, 30, 20, 20),
    time = c(1, 1, 2, 2)
  )

  out <- rates_aggregate(rates, "age_group")

  expect_equal(nrow(out), 2)
  # group "a": person_days-weighted mean clinical = (1*10 + 3*30) / 40 = 2.5
  expect_equal(out$clinical[out$age_group == "a"], 2.5)
  expect_equal(out$person_days[out$age_group == "a"], 40)
  expect_equal(out$time[out$age_group == "a"], 1)
})

test_that("prevalence_aggregate averages by group", {
  x <- data.frame(
    age_group = c("a", "a", "b", "b"),
    prevalence = c(0.2, 0.4, 0.6, 0.8),
    time = c(1, 2, 1, 2)
  )

  out <- prevalence_aggregate(x, "age_group")

  expect_equal(nrow(out), 2)
  expect_equal(out$prevalence[out$age_group == "a"], 0.3)
  expect_equal(out$time[out$age_group == "a"], 1.5)
})
