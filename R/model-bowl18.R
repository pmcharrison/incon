#' Harmonicity
#'
#' Computes the harmonicity of a chord using the
#' percentage similarity measure of Gill & Purves (2009).
#'
#' @details
#' This percentage similarity measure corresponds to the percentage of the harmonics
#' that the chord holds in common with a harmonic series rooted on
#' the chord's fundamental frequency.
#'
#' While \insertCite{Gill2009;textual}{incon} originally presented this measure
#' to quantify the harmonicity of two-note chords (dyads)
#' \insertCite{Bowling2018;textual}{incon} subsequently demonstrated the application
#' of this measure to chords of arbitrary sizes.
#'
#' @note
#' The algorithm assumes that chord pitches are precisely
#' tuned to the just-tuned scale provided by Bowling et al. (2018).
#' This scale can be found at \code{\link{rational_scale}}.
#'
#' @param x Chord to analyse,
#' expressed as an integerish vector of MIDI note numbers,
#' or more generally as any valid input to \code{\link[hrep:pi_chord]{pi_chord}}.
#' @param tonic (Integerish scalar, default = 0L)
#' Tonic to use when defining the just-tuned scale.
#' @return (Numeric scalar)
#' The chord's harmonicity, defined as the proportion of harmonics
#' of the chord's fundamental frequency that coincide with the
#' harmonics of the chord tones.
#' @references
#' \insertRef{Bowling2018}{incon}
#' @examples
#' gill09_harmonicity(c(60, 64, 67))
#' gill09_harmonicity("60 64 67")
#' gill09_harmonicity(hrep::pi_chord(c(60, 64, 67)))
#' @export
gill09_harmonicity <- function(x, tonic = 0L) {
  checkmate::qassert(tonic, "X1")
  x <- hrep::pi_chord(x)
  chord <- rationalise_chord(x, tonic)
  fundamental <- gcd(chord)
  max_freq <- lcm(chord)
  chord_harmonics <- expand_harmonics(chord, max_freq)
  fundamental_harmonics <- expand_harmonics(fundamental, max_freq)
  mean(fundamental_harmonics %in% chord_harmonics)
}

#' Minimum frequency distance
#'
#' This function returns the minimum distance between
#' the fundamental frequencies of a chord,
#' after Bowling et al. (2018).
#' It makes no assumptions about the chord's tuning.
#' @param x Chord to analyse.
#' The default method assumes that the chord is expressed
#' as a numeric vector of frequencies.
#' Representations from the hrep package
#' (e.g. \code{\link[hrep]{pi_chord}()})
#' can be used to analyse chords expressed as MIDI note numbers.
#' @return (Numeric scalar)
#' The minimum distance between the fundamental frequencies of the chord,
#' in Hz.
#' @references
#' \insertRef{Bowling2018}{incon}
#' @rdname bowl18_min_freq_dist
#' @examples
#' bowl18_min_freq_dist(c(220, 440, 560)) # processed as frequencies
#' bowl18_min_freq_dist(hrep::fr_chord(c(220, 440, 560))) # same as above
#' bowl18_min_freq_dist(hrep::pi_chord(c(60, 64, 67))) # MIDI note numbers
#' @export
bowl18_min_freq_dist <- function(x) {
  UseMethod("bowl18_min_freq_dist")
}

#' @rdname bowl18_min_freq_dist
#' @export
bowl18_min_freq_dist.default <- function(x) {
  bowl18_min_freq_dist(hrep::fr_chord(x))
}

#' @rdname bowl18_min_freq_dist
#' @export
bowl18_min_freq_dist.fr_chord <- function(x) {
  if (length(x) < 2L)
    as.numeric(NA) else
      min(diff(as.numeric(x)))
}

expand_harmonics <- function(x, max_freq) {
  UseMethod("expand_harmonics")
}

#' @export
expand_harmonics.rational_chord <- function(x, max_freq) {
  purrr::map(seq_len(ncol(x)),
             ~ expand_harmonics(fraction(x[, .]), max_freq)) %>%
    Reduce(union, .)
}

#' @export
expand_harmonics.fraction <- function(x, max_freq) {
  stopifnot(is(max_freq, "fraction"))
  n <- 0L
  res <- new.env()
  while (TRUE) {
    n <- n + 1L
    harmonic <- reduce_fraction(x * c(n, 1L))
    if ((harmonic[1] / harmonic[2]) >
        (max_freq[1] / max_freq[2])) break
    res[[as.character(n)]] <- as.character(harmonic)
  }
  res <- as.character(as.list(res))
  names(res) <- NULL
  res
}

fraction <- function(x, ...) {
  UseMethod("fraction")
}

#' @export
fraction.numeric <- function(x, ...) {
  checkmate::qassert(x, "X2")
  class(x) <- "fraction"
  x
}

#' @export
as.character.fraction <- function(x, ...) {
  paste0(x[1], "/", x[2])
}

half <- function(x) {
  UseMethod("half")
}

#' @export
half.fraction <- function(x) {
  if (x[1] %% 2L == 0L) x[1] <- x[1] / 2L else x[2] <- x[2] * 2L
  x
}

double <- function(x) {
  UseMethod("double")
}

#' @export
double.fraction <- function(x) {
  if (x[2] %% 2L == 0L) x[2] <- x[2] / 2L else x[1] <- x[1] * 2L
  x
}

gcd <- function(x) {
  UseMethod("gcd")
}

#' @export
gcd.rational_chord <- function(x) {
  y <- c(numbers::mGCD(x[1, ]),
         numbers::mLCM(x[2, ]))
  fraction(y)
}

lcm <- function(x) {
  UseMethod("lcm")
}

#' @export
lcm.rational_chord <- function(x) {
  y <- c(numbers::mLCM(x[1, ]),
         numbers::mGCD(x[2, ]))
  fraction(y)
}

# These functions assume that we are in the key of C
# (i.e. pitch-class 0 is the tonic).

rational_chord <- function(x) {
  stopifnot(is.matrix(x), is.numeric(x), nrow(x) == 2L)
  class(x) <- "rational_chord"
  x
}

rationalise_chord <- function(x, tonic) {
  UseMethod("rationalise_chord")
}

#' @export
rationalise_chord.pi_chord <- function(x, tonic) {
  x <- hrep::tp(x, - tonic)
  octave <- floor(hrep::get_bass_pi(x) / 12)
  x <- hrep::tp(x, - 12 * octave)
  sapply(x, rationalise_pitch) %>% rational_chord
}

rationalise_pitch <- function(x) {
  checkmate::qassert(x, "X1")
  octave <- floor(x / 12)
  pitch_class <- x %% 12
  fraction <- rationalise_pitch_class(pitch_class)
  while (octave != 0) {
    if (octave < 0) {
      fraction <- half(fraction)
      octave <- octave + 1L
    } else if (octave > 0) {
      fraction <- double(fraction)
      octave <- octave - 1L
    }
  }
  fraction
}

rationalise_pitch_class <- function(pc) {
  checkmate::qassert(pc, "X1[0,12)")
  fraction(rational_scale[, pc + 1L])
}

#' Rational scale
#'
#' This defines the rational scale used when computing harmonicity with
#' \code{\link{gill09_harmonicity}}.
#' It is a matrix with 2 rows and 12 columns,
#' where the first row corresponds to fraction numerators,
#' and the second row corresponds to fraction denominators.
#' Column i identifes the interval of size (i - 1) semitones.
#' For example, column 8 identifies the perfect fifth
#' (7 semitones) as a 3:2 ratio.
#'
#' @docType data
#' @keywords data
rational_scale <- matrix(
  c(1, 16, 9, 6, 5, 4, 7, 3, 8, 5, 9, 15,
    1, 15, 8, 5, 4, 3, 5, 2, 5, 3, 5, 8),
  nrow = 2,
  byrow = TRUE
)
