context("test-gill09_harmonicity")

test_that("regression tests: dyads", {
  df <- read.csv(system.file("bowling-data/dyads.csv", package = "incon"),
                 stringsAsFactors = FALSE)
  chords <- purrr::pmap(list(df$pc_1, df$pc_2), ~ hrep::pi_chord(c(..1, ..2)))

  for (i in seq_along(chords)) {
    expect_equal(df$bowling_harm_sim[i],
                 gill09_harmonicity(chords[[i]]),
                 tolerance = 1e-3)
  }
})

test_that("regression tests: triads", {
  df <- read.csv(system.file("bowling-data/triads.csv", package = "incon"),
                 stringsAsFactors = FALSE)
  chords <- purrr::pmap(list(df$pc_1, df$pc_2, df$pc_3),
                        ~ hrep::pi_chord(c(..1, ..2, ..3)))

  set.seed(1)
  ind <- sample(length(chords), size = 20L)
  for (i in ind) {
    expect_equal(df$bowling_harm_sim[i],
                 gill09_harmonicity(chords[[i]]),
                 tolerance = 1e-3)
  }
})

test_that("regression tests: tetrads", {
  df <- read.csv(system.file("bowling-data/tetrads.csv", package = "incon"),
                 stringsAsFactors = FALSE)
  chords <- purrr::pmap(list(df$pc_1, df$pc_2, df$pc_3, df$pc_4),
                        ~ hrep::pi_chord(c(..1, ..2, ..3, ..4)))

  set.seed(1)
  ind <- sample(length(chords), size = 20L)
  for (i in ind) {
    expect_equal(df$bowling_harm_sim[i],
                 gill09_harmonicity(chords[[i]]),
                 tolerance = 1e-3)
  }
})

context("test-bowl18_min_freq_dist")

test_that("regression tests: dyads", {
  df <- read.csv(system.file("bowling-data/dyads.csv", package = "incon"),
                 stringsAsFactors = FALSE)
  fr <- purrr::pmap(list(df$f_1, df$f_2),
                    ~ hrep::fr_chord(c(..1, ..2)))
  min_fr_int_old <- df$bowling_min_freq_int
  min_fr_int_new <- purrr::map_dbl(fr, bowl18_min_freq_dist)

  for (i in seq_along(fr)) {
    expect_equal(min_fr_int_old[i],
                 min_fr_int_new[i])
  }
})

test_that("regression tests: triads", {
  df <- read.csv(system.file("bowling-data/triads.csv", package = "incon"),
                 stringsAsFactors = FALSE)
  fr <- purrr::pmap(list(df$f_1, df$f_2, df$f_3),
                    ~ hrep::fr_chord(c(..1, ..2, ..3)))
  min_fr_int_old <- df$bowling_min_freq_int
  min_fr_int_new <- purrr::map_dbl(fr, bowl18_min_freq_dist)

  for (i in seq_along(fr)) {
    expect_equal(min_fr_int_old[i],
                 min_fr_int_new[i])
  }
})

test_that("regression tests: tetrads", {
  df <- read.csv(system.file("bowling-data/tetrads.csv", package = "incon"),
                 stringsAsFactors = FALSE)
  fr <- purrr::pmap(list(df$f_1, df$f_2, df$f_3, df$f_4),
                    ~ hrep::fr_chord(c(..1, ..2, ..3, ..4)))
  min_fr_int_old <- df$bowling_min_freq_int
  min_fr_int_new <- purrr::map_dbl(fr, bowl18_min_freq_dist)

  for (i in seq_along(fr)) {
    expect_equal(min_fr_int_old[i],
                 min_fr_int_new[i])
  }
})

context("test-gcd")

test_that("examples", {
  x <- rational_chord(matrix(c(4, 5, 6,
                               1, 1, 1),
                             nrow = 2, byrow = TRUE))
  expect_equal(gcd(x), fraction(c(1, 1)))

  x <- rational_chord(matrix(c(3, 4, 5,
                               2, 3, 4),
                             nrow = 2, byrow = TRUE))

  expect_equal(gcd(x), fraction(c(1, 12)))
})

context("test-rationalise_chord")

test_that("examples", {
  expect_equal(
    rationalise_chord(hrep::pi_chord(c(0, 4, 7)), tonic = 0),
    rational_chord(matrix(c(1, 5, 3,
                            1, 4, 2),
                          nrow = 2, byrow = TRUE))
  )

  expect_equal(
    rationalise_chord(hrep::pi_chord(c(60, 64, 67)), tonic = 0),
    rational_chord(matrix(c(1, 5, 3,
                            1, 4, 2),
                          nrow = 2, byrow = TRUE))
  )

  expect_equal(
    rationalise_chord(hrep::pi_chord(c(60, 64, 67 + 12)), tonic = 0),
    rational_chord(matrix(c(1, 5, 3,
                            1, 4, 1),
                          nrow = 2, byrow = TRUE))
  )
})

context("test-rational_pitch_class")

test_that("examples", {
  expect_equal(rationalise_pitch_class(0), fraction(c(1, 1)))
  expect_equal(rationalise_pitch_class(7), fraction(c(3, 2)))
})

context("test-rational_pitch")

test_that("examples", {
  expect_equal(rationalise_pitch(7), fraction(c(3, 2)))
  expect_equal(rationalise_pitch(0), fraction(c(1, 1)))
  expect_equal(rationalise_pitch(12), fraction(c(2, 1)))
  expect_equal(rationalise_pitch(12 + 7), fraction(c(3, 1)))
  expect_equal(rationalise_pitch(12 + 6), fraction(c(14, 5)))
})

context("test-tonic")

test_that("examples", {
  expect_gt(gill09_harmonicity(c(0, 4, 7) + 0),
            gill09_harmonicity(c(0, 4, 7) + 2))

  expect_equal(gill09_harmonicity(c(0, 4, 7) + 0),
               gill09_harmonicity(c(0, 4, 7) + 2, tonic = 2))

  for (i in seq_len(10)) {
    tonic <- sample(11, 1)
    x1 <- sample(40, size = 3) %>% hrep::pi_chord()
    x2 <- x1 + tonic
    expect_equal(
      gill09_harmonicity(x1),
      gill09_harmonicity(x2, tonic = tonic)
    )
  }
})
