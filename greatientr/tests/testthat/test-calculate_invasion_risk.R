test_that("invasion risk calculation works", {
  expect_lte(calculate_invasion_risk(0.5, 0.5, 0.5, 33, 33, 34), 1)
  expect_gte(calculate_invasion_risk(0.5, 0.5, 0.5, 33, 33, 34), 0)
})
