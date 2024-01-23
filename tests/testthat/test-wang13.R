context("test-wang13")

test_that("from legacy implementation", {
  # C major
  expect_equal(
    roughness_wang(c(48, 64, 67)),
    0.67,
    tolerance = 1e-2
  )
  expect_equal(
    roughness_wang(c(48, 64, 67), include_phase_impact_factors = TRUE),
    0.11,
    tolerance = 1e-2
  )

  # C dim
  expect_equal(
    roughness_wang(c(48, 63, 66)),
    0.82,
    tolerance = 1e-2
  )
  expect_equal(
    roughness_wang(c(48, 63, 66), include_phase_impact_factors = TRUE),
    0.10,
    tolerance = 1e-2
  )
})
