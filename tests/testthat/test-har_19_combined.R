context("test-har_19_composite")

test_that("Dyads without chord size", {
  incon::incon("60 72", model = "har_19_composite") %>%
    expect_equal(c(har_19_composite = 3.0600220088801))

  incon::incon("60 67", model = "har_19_composite") %>%
    expect_equal(c(har_19_composite = 2.46553060557208))

  incon::incon("60 65", model = "har_19_composite") %>%
    expect_equal(c(har_19_composite = 2.14697909722111))
})

test_that("Dyads with chord size", {
  incon::incon("60 72", model = "har_19_composite",
               par = list(har_19_composite = list(chord_size = TRUE))) %>%
    expect_equal(c(har_19_composite = 3.90455740609129))

  incon::incon("60 67", model = "har_19_composite",
               par = list(har_19_composite = list(chord_size = TRUE))) %>%
    expect_equal(c(har_19_composite = 3.31006600278327))

  incon::incon("60 65", model = "har_19_composite",
               par = list(har_19_composite = list(chord_size = TRUE))) %>%
    expect_equal(c(har_19_composite = 2.99151449443231))
})

test_that("Misc without chord size", {
  incon::incon("67 71 76", model = "har_19_composite") %>%
    expect_equal(c(har_19_composite = 1.65796888268932))

  incon::incon("67 68 71 73 76", model = "har_19_composite") %>%
    expect_equal(c(har_19_composite = 0.144723034155414))

  incon::incon("67 69 71 74 76", model = "har_19_composite") %>%
    expect_equal(c(har_19_composite = 1.06124734112462))
})

test_that("Misc with chord size", {
  incon::incon("67 71 76", model = "har_19_composite",
               par = list(har_19_composite = list(chord_size = TRUE))) %>%
    expect_equal(c(har_19_composite = 2.92477197850611))

  incon::incon("67 68 71 73 76", model = "har_19_composite",
               par = list(har_19_composite = list(chord_size = TRUE))) %>%
    expect_equal(c(har_19_composite = 2.25606152718341))

  incon::incon("67 69 71 74 76", model = "har_19_composite",
               par = list(har_19_composite = list(chord_size = TRUE))) %>%
    expect_equal(c(har_19_composite = 3.17258583415261))
})

