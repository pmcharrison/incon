context("test-cluster_chords")

library(magrittr)

# These are cases where there are too many tones masking each other,
# producing an empty complex sonority.

test_that("examples", {
  chord_1 <-  c(57, 60, 61, 62, 64, 65)
  chord_2 <-  c(57, 60, 61, 62, 63, 64, 65)

  complex_sonor(chord_1) %>% expect_equal(0)
  pure_sonor(chord_1) %>% expect_equal(0)
  multiplicity(chord_1) %>% expect_equal(0)

  expect_equal(
    pitch_commonality(
      chord_1, chord_2
    ),
    as.numeric(NA)
  )
  expect_equal(
    pitch_distance(
      chord_1, chord_2
    ),
    0
  )
})
context("test-complex-sonor")

test_that("testing against legacy code", {
  test <- function(x, y, ...) {
    expect_equal(complex_sonor(x, ...), y, tolerance = 1e-5)
  }

  test(c(60, 64, 67), 0.309965)
  test(c(60, 61, 62), 0.0007320671)
  test(c(80, 83, 86), 0.181884)
  test(c(1, 4, 7), 0)
  test(c(40, 54, 67), 0.44699)

  test(c(60, 64, 67), 0.3045464, par = parn94_params(unit_amplitude_in_dB = 50))
  test(c(61, 62, 63), 0.001448646, par = parn94_params(unit_amplitude_in_dB = 70))

  test(c(60, 64, 67), 0.2271383, par = parn94_params(template_num_harmonics = 5))
  test(c(60, 64, 67), 0.1523711, par = parn94_params(template_roll_off = 2))

  # HarmonyParncutt::get_parncutt_sonority_analysis(
  #   c(60, 64, 67),
  #   parncutt_params = HarmonyParncutt::get_parncutt_params(
  #     template_roll_off = 2),
  #   cache = FALSE)
})
context("test-helpers")

library(magrittr)

test_that("get_pure_tone_height matches figures given in Parncutt & Strasburger (1994)", {
  expect_equal(round(get_pure_tone_height(0)), 0)
  expect_equal(round(get_pure_tone_height(16)), 36)
})

test_that("Replicate calculation on pg. 101 of Parncutt & Strasburger (1994)", {
  expect_equal(get_partial_masking_level(
    masker_auditory_level = 50,
    masker_pure_tone_height = get_pure_tone_height(kHz = 0.4),
    maskee_auditory_level = 60,
    maskee_pure_tone_height = get_pure_tone_height(kHz = 0.5),
    k_m = 12
  ) %>% as.numeric %>% round,
  33)
  expect_equal(get_partial_masking_level(
    maskee_auditory_level = 50,
    maskee_pure_tone_height = get_pure_tone_height(kHz = 0.4),
    masker_auditory_level = 60,
    masker_pure_tone_height = get_pure_tone_height(kHz = 0.5),
    k_m = 12
  ) %>% as.numeric %>% round,
  43)
})

test_that("Check matrix aspects of partial_masking_level", {
  mat <- get_partial_masking_level(
    masker_auditory_level = c(50, 60),
    masker_pure_tone_height = get_pure_tone_height(c(0.4, 0.5)),
    maskee_auditory_level = c(50, 60),
    maskee_pure_tone_height = get_pure_tone_height(c(0.4, 0.5)),
    k_m = 12
  )
  expect_equal(
    mat[1, 2] %>% round, 43
  )
  expect_equal(
    mat[2, 1] %>% round, 33
  )
})

test_that("get_overall_masking_level", {
  expect_equal(get_overall_masking_level(
    auditory_level = 50,
    pure_tone_height = get_pure_tone_height(0.5),
    k_m = 12
  ), 0)
})

test_that("get_pure_tone_audible_level", {
  expect_equal(
    get_pure_tone_audible_level(auditory_level = 20, overall_masking_level = 10),
    10
  )
  expect_equal(
    get_pure_tone_audible_level(auditory_level = 30, overall_masking_level = 50),
    0
  )
})

test_that("get_pure_tone_audibility", {
  expect_equal(
    get_pure_tone_audibility(pure_tone_audible_level = 0, al_0 = 15),
    0
  )
  expect_gt(
    get_pure_tone_audibility(pure_tone_audible_level = 20, al_0 = 15),
    0
  )
  expect_gt(
    get_pure_tone_audibility(pure_tone_audible_level = 30, al_0 = 15),
    get_pure_tone_audibility(pure_tone_audible_level = 20, al_0 = 15)
  )
})
context("test-multiplicity")

test_that("testing against legacy code", {
  test <- function(x, y, ...) {
    expect_equal(multiplicity(x, ...), y, tolerance = 1e-5)
  }

  test(c(60, 64, 67), 2.843946)
  test(c(60, 61, 62), 1.293558)
  test(c(80, 83, 86), 3.242737)
  test(c(40, 54, 67), 2.220517)

  test(c(60, 64, 67), 2.835933, par = parn94_params(unit_amplitude_in_dB = 50))
  test(c(61, 62, 63), 1.293558, par = parn94_params(unit_amplitude_in_dB = 70))

  test(hrep::sparse_pi_spectrum(c(60, 64, 67), roll_off = 2, digits = 0),
       2.899631)

  # HarmonyParncutt::get_parncutt_sonority_analysis(
  #   c(60, 64, 67),
  #   # amplitude = 70,
  #   # parncutt_params = HarmonyParncutt::get_parncutt_params(),
  #   midi_params = HarmonyParncutt::get_midi_params(roll_off = 2),
  #   cache = FALSE)$multiplicity
})
context("test-pitch_commonality")

test_that("pitch_commonality", {
  expect_equal(
    pitch_commonality(c(60, 64, 67), c(60, 64, 67)),
    1
  )
  expect_gt(
    # G major should be closer to C major than F# major is to C major
    pitch_commonality(c(60, 64, 67), c(59, 62, 67)),
    pitch_commonality(c(60, 64, 67), c(61, 66, 68))
  )
  expect_gt(
    # G major vs C# major
    pitch_commonality(c(60, 64, 67), c(59, 62, 67)),
    pitch_commonality(c(60, 64, 67), c(61, 65, 68))
  )
  expect_gt(
    # G major vs C transposed
    pitch_commonality(c(60, 64, 67), c(48, 76, 79)),
    pitch_commonality(c(60, 64, 67), c(59, 62, 67))
  )
  # These numbers are taken from previous versions of this package,
  # and have not been compared to other literature/software
  expect_equal(
    pitch_commonality(c(60, 64, 67), c(48, 76, 79)),
    0.894901857522212,
    tolerance = 1e-4
  )
  expect_equal(
    pitch_commonality(c(60, 64, 67), c(59, 62, 67)),
    0.349625432417314
  )
})
context("test-pitch_distance")

test_that("pitch_distance", {
  expect_equal(
    pitch_distance(c(60, 64, 67), c(60, 64, 67)),
    0
  )
  expect_gt(
    # Presumably C# major should be closer in pitch to
    # C major than e.g. F major
    pitch_distance(c(60, 64, 67), c(65, 69, 72)),
    pitch_distance(c(60, 64, 67), c(61, 65, 68))
  )
  # These numbers are taken from previous versions of this package,
  # and have not been compared to other literature/software
  expect_equal(
    pitch_distance(c(60, 64, 67), c(65, 69, 72)),
    3.86723877405512
  )
  expect_equal(
    pitch_distance(c(60, 64, 67), c(65, 63, 83)),
    37.8133050960468
  )
})
context("test-pitch_salience")

library(magrittr)

test_that("pitch_salience", {
  x <- .parn94()
  x$combined_spectrum <- data.frame(pitch = 1:5, salience = 1)
  x$par <- list(min_midi = 0, max_midi = 10)
  expect_equal(
    pitch_salience(x) %>% as.numeric,
    c(0, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0)
  )
})
context("test-pure-sonor")

test_that("testing against legacy code", {
  test <- function(x, y, ...) {
    expect_equal(pure_sonor(x, ...), y, tolerance = 1e-5)
  }

  test(c(60, 64, 67), 0.6157366)
  test(c(60, 61, 62), 0.005490503)
  test(c(80, 83, 86), 0.6535714)
  test(c(1, 4, 7), 0)
  test(c(40, 54, 67), 0.5373542)

  test(c(60, 64, 67), 0.6061362, par = parn94_params(unit_amplitude_in_dB = 50))
  test(c(61, 62, 63), 0.01086485, par = parn94_params(unit_amplitude_in_dB = 70))

  test(hrep::sparse_pi_spectrum(c(60, 64, 67), roll_off = 2, digits = 0),
       0.6136948)

  # HarmonyParncutt::get_parncutt_sonority_analysis(
  #   c(60, 64, 67),
  #   parncutt_params = HarmonyParncutt::get_parncutt_params(),
  #   midi_params = HarmonyParncutt::get_midi_params(roll_off = 2),
  #   cache = FALSE)$pure_sonorousness
})
