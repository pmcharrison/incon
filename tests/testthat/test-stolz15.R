context("test-misc")

context("stolzenburg")

library(magrittr)

test_that("approximating fractions", {
  # These examples are taken from Table 1 of
  # 10.1080/17459737.2015.1033024
  expect_equal(fraction(1.059, 0.01), c(16, 15))
  expect_equal(fraction(1.122, 0.01), c(9, 8))
  expect_equal(fraction(1.189, 0.01), c(6, 5))
  expect_equal(fraction(1.260, 0.01), c(5, 4))
  expect_equal(fraction(1.335, 0.011), c(4, 3))
  expect_equal(fraction(1.414, 0.011), c(7, 5))
  expect_equal(fraction(1.498, 0.011), c(3, 2))
  expect_equal(fraction(1.587, 0.011), c(8, 5))
})

test_that("double fraction", {
  expect_equal(double_fraction(c(3, 4)), c(3, 2))
  expect_equal(double_fraction(c(3, 7)), c(6, 7))
})

test_that("double fraction", {
  expect_equal(half_fraction(c(6, 5)), c(3, 5))
  expect_equal(half_fraction(c(3, 7)), c(3, 14))
})

test_that("get_rational_interval_2", {
  expect_equal(get_rational_interval(0), c(1, 1))
  expect_equal(get_rational_interval(12), c(2, 1))
  expect_equal(get_rational_interval(-3), c(5, 6))
  expect_equal(get_rational_interval(-9), c(3, 5))
  expect_equal(get_rational_interval(-6), c(7, 10))
})

test_that("least common multiple", {
  expect_equal(lcm(c(4, 6)), 12)
  expect_equal(lcm(c(21, 6)), 42)
  expect_equal(lcm(c(8, 9, 21)), 504)
})

test_that("smooth_log_periodicity", {
  expect_equal(smooth_log_periodicity(c(0, 3, 9)) %>% round(digits = 1),
               3.7)
  expect_equal(smooth_log_periodicity(c(48, 64, 67)), 1)
  expect_equal(smooth_log_periodicity(c(0, 3, 7)), log2(10))
  expect_equal(smooth_log_periodicity(c(0:11)) %>% round(digits = 1), 7.4)
})

context("test-regression")

test_that("regression tests", {
  df <- read.csv(system.file("stolz15/data-formatted.csv",
                             package = "incon"),
                 stringsAsFactors = FALSE)
  chords <- lapply(strsplit(gsub("\\{|\\}", "", df$chord),
                            split = ","),
                   as.integer)

  for (i in seq_along(chords)) {
    expect_equal(
      smooth_log_periodicity(chords[[i]]),
      df$periodicity.stolz_smooth_t2_log[[i]],
      tolerance = 1e-3
    )
  }
})
