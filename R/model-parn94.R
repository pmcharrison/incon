add_combined_spectrum <- function(x, par) {
  df <- merge_spectra(x$pure_spectrum, x$complex_spectrum)
  df$combined_audibility <- pmax(df$pure_tone_audibility,
                                 df$complex_tone_audibility,
                                 0, na.rm = TRUE)
  df$salience <- get_tone_salience(df$combined_audibility, par$k_s)
  x$combined_spectrum <- df[, c("pitch", "combined_audibility", "salience")]
  x
}

merge_spectra <- function(pure_spectrum, complex_spectrum) {
  merge(pure_spectrum[, c("pitch", "pure_tone_audibility")],
        complex_spectrum[, c("pitch", "complex_tone_audibility")],
        by = "pitch",
        all = TRUE)
}
add_complex_spectrum <- function(x, par) {
  spectrum <- x$pure_spectrum[, c("pitch", "pure_tone_audibility")]
  template <- get_template(par)
  df <- tibble::tibble(
    pitch = seq(from = par$min_midi,
                to = par$max_midi),
    complex_tone_audibility = purrr::map_dbl(.data$pitch,
                                             template_match,
                                             template,
                                             spectrum,
                                             par)
  )
  x$complex_spectrum <- df[df$complex_tone_audibility > 0, ]
  x
}

template_match <- function(fundamental, template, spectrum, par) {
  transposed_template <- tibble::tibble(pitch = template$interval + fundamental,
                                        weight = template$weight)
  df <- merge(transposed_template, spectrum,
              all.x = FALSE, all.y = FALSE)
  ((sum(sqrt(df$weight * df$pure_tone_audibility))) ^ 2) / par$k_t
}

get_template <- function(par) {
  hrep::pi_chord(0) %>%
    {hrep::sparse_pi_spectrum(.,
                              num_harmonics = par$template_num_harmonics,
                              roll_off = par$template_roll_off,
                              digits = 0)} %>%
    (tibble::as_tibble) %>%
    {magrittr::set_names(., c("interval", "weight"))}
}
# @param res Where results are stored
# @param x Input spectrum
add_pure_spectrum <- function(res, x, par) {
  y <- tibble::tibble(
    pitch = hrep::pitch(x),
    amplitude = hrep::amp(x),
    kHz = hrep::midi_to_freq(.data$pitch) / 1000,
    level = hrep::amplitude_to_dB(.data$amplitude, par$unit_amplitude_in_dB),
    free_field_threshold = get_free_field_threshold(.data$kHz),
    auditory_level = pmax(.data$level - .data$free_field_threshold, 0),
    pure_tone_height = get_pure_tone_height(.data$kHz),
    overall_masking_level = get_overall_masking_level(.data$auditory_level,
                                                      .data$pure_tone_height,
                                                      k_m = par$k_m),
    pure_tone_audible_level = get_pure_tone_audible_level(.data$auditory_level,
                                                          .data$overall_masking_level),
    pure_tone_audibility = get_pure_tone_audibility(.data$pure_tone_audible_level,
                                                    al_0 = par$al_0)
  )
  res$pure_spectrum <- y[y$pure_tone_audibility > 0, ]
  res
}
#' Get complex sonorousness
#'
#' Computes the complex sonorousness of a sound, after
#' \insertCite{Parncutt1994;textual}{incon}.
#' @param x Object to analyse.
#' @param k_c Parncutt & Strasburger (1994) set this to 0.2 (p. 105)
#' @param ... Further parameters to pass to \code{\link{parn94}()}.
#' @return Complex sonorousness, a numeric scalar.
#' @rdname complex_sonor
#' @references
#' \insertAllCited{}
#' @export
complex_sonor <- function(x, k_c = parn94_params()$k_c, ...) {
  UseMethod("complex_sonor")
}

#' @rdname complex_sonor
#' @export
complex_sonor.parn94 <- function(x, k_c = parn94_params()$k_c, ...) {
  audibility <- x$complex_spectrum$complex_tone_audibility
  if (length(audibility) == 0)
    0 else
      k_c * max(audibility)
}

#' @rdname complex_sonor
#' @export
complex_sonor.default <- function(x, k_c = parn94_params()$k_c, ...) {
  x <- parn94(x, ...)
  complex_sonor(x, k_c = k_c)
}
#' Get free field threshold
#'
#' Returns the free-field threshold (dB SPL) of hearing in quiet for pure tones
#' of given frequencies.
#' This is the minimum sound level at which a pure tone at that frequency
#' will be heard.
#' Corresponds to Equation 2 in \insertCite{Parncutt1994;textual}{incon}.
#' @param kHz Numeric vector of frequencies in kHz.
#' @return Numeric vector of corresponding free-field thresholds in dB SPL.
#' @references
#' \insertAllCited{}
#' @export
get_free_field_threshold <- function(kHz) {
  3.64 * (kHz ^ -0.8) -
    6.5 * exp(- 0.6 * (kHz - 3.3) ^ 2) +
    (10 ^ (-3)) * (kHz ^ 4)
}

#' Get pure-tone height
#'
#' Returns the pure-tone heights (a.k.a. critical-band rates)
#' of pure tones of given frequencies.
#' Equation 3 in \insertCite{Parncutt1994;textual}{incon}.
#' @param kHz Numeric vector of frequencies in kHz.
#' @return Numeric vector of corresponding pure-tone heights,
#' with units of equivalent rectangular bandwidths (ERBs).
#' @references
#' \insertAllCited{}
#' @export
get_pure_tone_height <- function(kHz) {
  H1 <- 11.17
  H0 <- 43.0
  f1 <- 0.312
  f2 <- 14.675
  H1 * log((kHz + f1) / (kHz + f2)) + H0
}

#' Get partial masking level
#'
#' Returns the effective reduction in dB of the audible level
#' of a masked pure tone (maskee) on account of a masking pure tone (masker).
#' Equation 4 in \insertCite{Parncutt1994;textual}{incon}.
#' @param masker_auditory_level Numeric vector of masker auditory levels.
#' @param masker_pure_tone_height Numeric vector of masker pure tone heights.
#' @param maskee_auditory_level Numeric vector of maskee auditory levels.
#' @param maskee_pure_tone_height Numeric vector of maskee pure tone heights.
#' @param k_m Parameter \code{k_m} in \insertCite{Parncutt1994;textual}{incon}.
#' represents the masking pattern gradient for a pure tone,
#' with units of dB per critical band.
#' Parncutt & Strasburger use a value of 12 in their examples,
#' but imply that 12-18 is a typical range of values for this parameter.
#' @return Matrix where element [i, j] gives the level of masking
#' for masker j on maskee i.
#' @references
#' \insertAllCited{}
#' @export
get_partial_masking_level <- function(masker_auditory_level,
                                      masker_pure_tone_height,
                                      maskee_auditory_level,
                                      maskee_pure_tone_height,
                                      k_m) {
  assertthat::assert_that(
    length(masker_auditory_level) == length(masker_pure_tone_height),
    length(maskee_auditory_level) == length(maskee_pure_tone_height)
  )
  ncol <- length(masker_auditory_level)
  nrow <- length(maskee_auditory_level)
  # Masker matrices
  masker_auditory_level_matrix <- matrix(
    data = rep(masker_auditory_level, each = nrow),
    nrow = nrow, ncol = ncol, byrow = FALSE
  )
  masker_pure_tone_height_matrix <- matrix(
    data = rep(masker_pure_tone_height, each = nrow),
    nrow = nrow, ncol = ncol, byrow = FALSE
  )
  # Maskee matrix
  maskee_pure_tone_height_matrix <- matrix(
    data = rep(maskee_pure_tone_height, each = ncol),
    nrow = nrow, ncol = ncol, byrow = TRUE
  )
  # Result
  masker_auditory_level_matrix -
    k_m * abs(
      masker_pure_tone_height_matrix - maskee_pure_tone_height_matrix
    )
}

#' Get overall masking level
#'
#' Returns overall masking levels for a set of pure tones
#' that are assumed to be playing simultaneously.
#' Corresponds to Parncutt & Strasburger (1994) Equation 5.
#' @param auditory_level Numeric vector of auditory levels
#' @param pure_tone_height Numeric vector of pure tone heights
#' @param k_m See \code{\link{parn94_params}()}.
#' @return Numeric vector of overall masking levels (dB).
#' @references
#' \insertAllCited{}
#' @export
get_overall_masking_level <- function(auditory_level,
                                      pure_tone_height,
                                      k_m) {
  partial_mask_matrix <- get_partial_masking_level(
    masker_auditory_level = auditory_level,
    masker_pure_tone_height = pure_tone_height,
    maskee_auditory_level = auditory_level,
    maskee_pure_tone_height = pure_tone_height,
    k_m = k_m
  )
  # Tones don't mask themselves
  diag(partial_mask_matrix) <- 0
  # Sum over maskers to find the masking for each maskee
  apply(
    partial_mask_matrix, 1,
    function(x) {
      max(c(
        20 * log(sum(10 ^ (x / 20)), base = 10),
        0
      ))
    }
  )
}

#' Get audible level
#'
#' Returns the audible level for set of pure tones subject
#' to a given masking pattern.
#' Corresponds to Equation 6 of \insertCite{Parncutt1994;textual}{incon}.
#' @param auditory_level Numeric vector of auditory levels for
#' a set of pure tones (dB).
#' @param overall_masking_level Numeric vector of overall masking levels
#' for a set of pure tones (dB).
#' @return Numeric vector of audible levels (dB).
#' @references
#' \insertAllCited{}
#' @export
get_pure_tone_audible_level <- function(auditory_level, overall_masking_level) {
  assertthat::assert_that(length(auditory_level) == length(overall_masking_level))
  pmax(0, auditory_level - overall_masking_level)
}

#' Get audibility
#'
#' Returns the audibility of a set of pure tone components as a
#' function of their audible levels.
#' Corresponds to Equation 7 of \insertCite{Parncutt1994;textual}{incon}.
#' @param pure_tone_audible_level Numeric vector of audible levels (dB).
#' @param al_0 constant (see Equation 7 of Parncutt & Strasburger (1994)).
#' @return Numeric vector of pure tone audibilities.
#' @references
#' \insertAllCited{}
#' @export
get_pure_tone_audibility <- function(pure_tone_audible_level, al_0) {
  1 - exp(- pure_tone_audible_level / al_0)
}

#' Get tone salience
#'
#' Gets the salience of different tones within a sonority with
#' reference to the combined (complex and pure) spectrum.
#' Salience can be interpreted as the probability of consciously
#' perceiving a given pitch.
#' @param combined_audibility Numeric vector corresponding to the
#' audibilities of each tone in the combined (pure and complex) spectrum.
#' @param k_s Numeric scalar; \insertCite{Parncutt1994;textual}{incon}
#' set this to 0.5.
#' @return Numeric vector of saliences of the same length as
#' \code{combined_audibility}, giving the salience of each respective tone.
#' @references
#' \insertAllCited{}
#' @export
get_tone_salience <- function(combined_audibility, k_s) {
  if (length(combined_audibility) == 0) {
    numeric()
  } else {
    a_max <- max(combined_audibility)
    m_prime <- sum(combined_audibility) / a_max
    m <- m_prime ^ k_s
    (combined_audibility / a_max) *
      (m / m_prime)
  }
}
#' @importFrom magrittr "%>%"
NULL

#' @importFrom stats cor
NULL

#' @importFrom methods .valueClassTest
NULL

#' @importFrom methods new
NULL

# `.` <- NULL

utils::globalVariables(c(".", ".data"), package = "incon")
#' Get multiplicity
#'
#' Computes the multiplicity of a sound, after
#' \insertCite{Parncutt1994;textual}{incon}.
#' @param x Object to analyse.
#' @param k_s Numeric scalar, parameter from Parncutt & Strasburger (1994).
#' @param ... Further parameters to pass to \code{\link{incon}()}.
#' @return Multiplicity, a numeric scalar.
#' @rdname multiplicity
#' @references
#' \insertAllCited{}
#' @export
multiplicity <- function(x, k_s = parn94_params()$k_s, ...) {
  UseMethod("multiplicity")
}

#' @rdname multiplicity
#' @export
multiplicity.parn94 <- function(x, k_s = parn94_params()$k_s, ...) {
  audibility <- x$combined_spectrum$combined_audibility
  if (length(audibility) == 0) {
    0
  } else {
    a_max <- max(audibility)
    m_prime <- sum(audibility) / a_max
    m <- m_prime ^ k_s
    m
  }
}

#' @rdname multiplicity
#' @export
multiplicity.default <- function(x, k_s = parn94_params()$k_s, ...) {
  x <- parn94(x, ...)
  multiplicity(x, k_s = k_s)
}
#' Model parameters
#'
#' This function compiles parameters for Parncutt's psychoacoustic model.
#' The parameters are defined with reference to
#' \insertCite{Parncutt1994;textual}{incon}.
#' @param unit_amplitude_in_dB (Numeric scalar) Describes the number of decibels
#' to assign to a spectral component of amplitude 1.
#' By default, amplitude 1 corresponds to the amplitude of each chord pitch's
#' fundamental frequency (though this can be changed by expressing input chords
#' as pitch-class sparse spectra, see \code{\link[hrep]{sparse_pi_spectrum}()}).
#' @param template_num_harmonics (Integerish scalar) Number of harmonics to
#' include in the spectral template, including the fundamental frequency.
#' @param template_roll_off (Numeric scalar) Roll-off rate for the
#' harmonic template. This parameter is passed to
#' \code{\link[hrep]{sparse_pi_spectrum}()}.
#' @param k_t (Numeric scalar) Relative perceptual weighting of complex versus pure tones.
#' @param k_p (Numeric scalar) Scaling coefficient for pure sonorousness.
#' @param k_c (Numeric scalar) Scaling coefficient for complex sonorousness.
#' @param k_s (Numeric scalar) Scaling coefficient for multiplicity.
#' @param k_m (Numeric scalar) Gradient of a pure tone's masking pattern.
#' @param al_0 (Numeric scalar) Audibility saturation rate.
#' @param min_midi (Numeric scalar) Lowest MIDI pitch considered by the model.
#' @param max_midi (Numeric scalar) Highest MIDI pitch considered by the model.
#' @return A list of parameters which can be passed to analysis functions
#' such as \code{\link{parn94}}.
#' @references
#' \insertAllCited{}
#' @export
parn94_params <- function(
  unit_amplitude_in_dB = 60,
  template_num_harmonics = 11,
  template_roll_off = 1,
  k_t = 3,
  k_p = 0.5,
  k_c = 0.2,
  k_s = 0.5,
  k_m = 12,
  al_0 = 15,
  min_midi = 0,
  max_midi = 120
) {
  as.list(environment())
}
#' Parncutt & Strasburger (1994)
#'
#' This function analyses a sonority using Richard Parncutt's
#' psychoacoustic model of harmony, as described in
#' \insertCite{Parncutt1994;textual}{incon}.
#'
#' @param x Object to analyse,
#' which will be coerced to an object of class
#' \code{\link[hrep]{sparse_pi_spectrum}}.
#' Various input types are possible:
#' * Numeric vectors will be treated as vectors of MIDI note numbers,
#' which will be expanded into their implied harmonics.
#' * A two-element list can be used to define a harmonic spectrum.
#' The first element should be a vector of MIDI note numbers,
#' the second a vector of amplitudes.
#' * The function also accepts classes from the \code{hrep} package,
#' such as produced by \code{\link[hrep]{pi_chord}()} and
#' \code{\link[hrep]{sparse_pi_spectrum}()}.
#'
#' @param par Parameter list as created by \code{\link{parn94_params}()}.
#'
#' @param ... Parameters to pass to \code{\link[hrep]{sparse_pi_spectrum}}.
#' * \code{num_harmonics}: Number of harmonics to use when expanding
#' chord tones into their implied harmonics.
#' * \code{roll_off}: Rate of amplitude roll-off for the harmonics.
#'
#' @return An list of class \code{parn94}, comprising the following components:
#' \item{pure_spectrum}{A tibble describing the sonority's pure spectrum.
#' The pure spectrum is a spectral representation of the input sound
#' after auditory masking, but before pattern matching.}
#' \item{pure_spectrum}{A tibble describing the sonority's complex spectrum.
#' The complex spectrum is created from the pure spectrum through
#' harmonic template matching.}
#' \item{combined_spectrum}{A tibble describing the sonority's combined spectrum.
#' The combined spectrum corresponds to the combination of the
#' pure and complex spectra.}
#' \item{par}{A list comprising the parameters used to perform the analysis,
#' as created by \code{\link{parn94_params}()}.}
#'
#' @references
#' \insertAllCited{}
#'
#' @rdname parn94
#'
#' @md
#'
#' @export
parn94 <- function(x, par = parn94_params(), ...) {
  UseMethod("parn94")
}

#' @rdname parn94
#' @export
parn94.default <- function(x, par = parn94_params(), ...) {
  x <- hrep::sparse_pi_spectrum(x, digits = 0, ...)
  parn94(x, par = par)
}

#' @rdname parn94
#' @export
parn94.sparse_pi_spectrum <- function(x, par = parn94_params(), ...) {
  x <- preprocess_spectrum(x, par)
  .parn94() %>%
    add_pure_spectrum(x, par) %>%
    add_complex_spectrum(par) %>%
    add_combined_spectrum(par) %>%
    add_par(par)
}

preprocess_spectrum <- function(x, par) {
  if (!hrep::is.equal_tempered(x)) stop("input must be equal-tempered")
  x[hrep::pitch(x) >= par$min_midi &
      hrep::pitch(x) <= par$max_midi, ]
}

add_par <- function(x, par) {
  x$par <- par
  x
}

.parn94 <- function() {
  x <- list()
  class(x) <- "parn94"
  x
}
#' Get pitch commonality
#'
#' Gets the pitch commonality between two sonorities, after
#' \insertCite{Parncutt1994;textual}{incon}.
#'
#' @param x The first sonority to compare, passed to \code{\link{pitch_salience}()}.
#' Typically will be a numeric vector of MIDI pitches.
#'
#' @param y The second sonority to compare, passed to \code{\link{pitch_salience}()}.
#' Typically will be a numeric vector of MIDI pitches.
#'
#' @param ... Further arguments to pass to \code{\link{pitch_salience}()}.
#'
#' @return Pitch commonality, as a numeric scalar.
#'
#' @references
#' \insertAllCited{}
#'
#' @export
pitch_commonality <- function(x, y, ...) {
  s1 <- pitch_salience(x, ...)
  s2 <- pitch_salience(y, ...)
  if (length(s1) == 0L || length(s2) == 0L) return(as.numeric(NA))

  if (attr(s1, "min_midi") != attr(s2, "min_midi") ||
      attr(s1, "max_midi") != attr(s2, "max_midi"))
    stop("x and y must be created with identical 'min_midi' and max_midi' ",
         "parameters")

  if (all(s1 == 0 ) || all(s2 == 0))
    as.numeric(NA) else
      cor(s1, s2)
}
#' Get pitch distance
#'
#' Gets the pitch distance between two sonorities, after
#' \insertCite{Parncutt1994;textual}{incon}.
#'
#' @param x The first sonority to compare, passed to \code{\link{pitch_salience}()}.
#' Typically will be a numeric vector of MIDI pitches.
#'
#' @param y The second sonority to compare, passed to \code{\link{pitch_salience}()}.
#' Typically will be a numeric vector of MIDI pitches.
#'
#' @param ... Further arguments to pass to \code{\link{pitch_salience}()}.
#'
#' @return Pitch distance, as a numeric scalar.
#'
#' @references
#' \insertAllCited{}
#' @export
pitch_distance <- function(x, y, ...) {
  s1 <- pitch_salience(x, ...)
  s2 <- pitch_salience(y, ...)
  if (length(s1) == 0L || length(s2) == 0L) return(as.numeric(NA))

  min_midi <- attr(s1, "min_midi")
  max_midi <- attr(s1, "max_midi")
  if (min_midi != attr(s2, "min_midi") ||
      max_midi != attr(s2, "max_midi"))
    stop("x and y must be created with identical 'min_midi' and max_midi' ",
         "parameters")

  # We define some matrices that will allow us to vectorise our calculation -
  # see Equation 17 of Parncutt & Strasburger (1994).
  # Element [i, j] of each matrix corresponds to one combination of P / P'
  # in Equation 17.

  dim <- length(s1)
  m1 <- matrix(data = rep(seq(from = min_midi, to = max_midi), each = dim),
               nrow = dim, byrow = TRUE)
  m2 <- matrix(data = rep(seq(from = min_midi, to = max_midi), each = dim),
               nrow = dim, byrow = FALSE)
  dist <- abs(m1 - m2)
  s1_mat <- matrix(data = rep(s1, each = dim), nrow = dim, byrow = TRUE)
  s2_mat <- matrix(data = rep(s2, each = dim), nrow = dim, byrow = FALSE)

  sum(s1_mat * s2_mat * dist) -
    sqrt(sum(s1_mat * t(s1_mat) * dist) *
           sum(t(s2_mat) * s2_mat * dist))
}
#' Get pitch salience
#'
#' Analyses the pitch salience of a sonority, after
#' \insertCite{Parncutt1994;textual}{incon}.
#' @param x Object to analyse, passed to \code{\link{parn94}()}.
#' @param ... Further arguments to pass to \code{\link{parn94}()}.
#' @return Returns a vector where each element describes
#' the salience of a different chromatic pitch.
#' The first element of this vector corresponds to the
#' \code{min_midi} argument from \code{\link{parn94_params}},
#' and the last element corresponds to the \code{max_midi} argument.
#' @references
#' \insertAllCited{}
#' @rdname pitch_salience
#' @export
pitch_salience <- function(x, ...) {
  UseMethod("pitch_salience")
}

#' @rdname pitch_salience
#' @export
pitch_salience.default <- function(x, ...) {
  x <- parn94(x, ...)
  pitch_salience(x)
}

#' @rdname pitch_salience
#' @export
pitch_salience.pitch_salience <- function(x, ...) {
  x
}

#' @rdname pitch_salience
#' @export
pitch_salience.parn94 <- function(x, ...) {
  vec <- numeric(x$par$max_midi - x$par$min_midi + 1)
  ind <- x$combined_spectrum$pitch - x$par$min_midi + 1
  val <- x$combined_spectrum$salience
  vec[ind] <- val
  .pitch_salience(vec, x$par$min_midi, x$par$max_midi)
}

.pitch_salience <- function(x, min_midi, max_midi) {
  class(x) <- "pitch_salience"
  attr(x, "min_midi") <- min_midi
  attr(x, "max_midi") <- max_midi
  x
}
#' Get pure sonorousness
#'
#' Computes the pure sonorousness of a sound, after
#' \insertCite{Parncutt1994;textual}{incon}.
#' @param x Object to analyse.
#' @param k_p Parncutt & Strasburger (1994) set this to 0.5 (p. 105).
#' @param ... Further parameters to pass to \code{\link{parn94}()}.
#' @return Pure sonorousness, a numeric scalar.
#' @rdname pure_sonor
#' @references
#' \insertAllCited{}
#' @export
pure_sonor <- function(x, k_p = parn94_params()$k_p, ...) {
  UseMethod("pure_sonor")
}

#' @rdname pure_sonor
#' @export
pure_sonor.parn94 <- function(x, k_p = parn94_params()$k_p, ...) {
  k_p * sqrt(sum(x$pure_spectrum$pure_tone_audibility ^ 2))
}

#' @rdname pure_sonor
#' @export
pure_sonor.default <- function(x, k_p = parn94_params()$k_p, ...) {
  x <- parn94(x, ...)
  pure_sonor(x, k_p = k_p)
}
