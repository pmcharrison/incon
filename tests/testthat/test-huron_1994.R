context("test-huron_1994")

test_that("examples from Huron (1994)", {
  expect_equal(huron_1994(0:11), - 0.198, tolerance = 1e-2)
  expect_equal(huron_1994(0:10), - 0.17, tolerance = 1e-2)
  expect_equal(huron_1994(c(0, 4, 7)), 2.22, tolerance = 1e-2)
  expect_equal(huron_1994(c(0, 3, 7)), 2.22, tolerance = 1e-2)
  expect_equal(huron_1994(c(0, 4, 8)), 1.16, tolerance = 1e-2)
  expect_equal(huron_1994(c(0, 3, 6)), 0.74, tolerance = 1e-2)
})
