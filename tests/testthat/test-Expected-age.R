test_that("Expected_age works", {
  expect_equal(ea(0, Inf, 100), 100)
  expect_lt(ea(0, 50, 100), 50)
  expect_equal(expected_age(rep(0, 100), rep(Inf, 100), 100), rep(100, 100))
})
