context("test-non-integer")

test_that("examples", {
  expect_error(
    incon("60 64 67.5", model = "gill_09_harmonicity"),
    "gill_09_harmonicity requires integer inputs"
  )

  # Continuous-pitch models default to 'x' parameter
  incon("60 64 67", model = "stolz_15_periodicity", x_int = "60 63 67") %>%
    expect_equal(incon("60 64 67", "stolz_15_periodicity"))

  # Non-continuous-pitch models default to 'x_int' parameter
  incon("60 64 67", model = "gill_09_harmonicity", x_int = "60 63 67") %>%
    expect_equal(incon("60 63 67", "gill_09_harmonicity"))
})
