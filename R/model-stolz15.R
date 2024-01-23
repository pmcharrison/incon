#' Smoothed log periodicity
#'
#' This function computes a chord's smoothed logarithmic periodicity,
#' after \insertCite{Stolzenburg2015;textual}{incon}.
#' @param x Sonority to analyse.
#' This will be coerced to an object of class \code{\link[hrep]{pi_chord}}.
#' Numeric inputs will be interpreted as MIDI note numbers.
#' @param d (numeric scalar):
#' Maximal allowed error in the algorithm's
#' interval approximation step, expressed as
#' a fraction of the original interval.
#' The default value, 0.011, corresponds to 'Rational Tuning II'
#' in Stolzenburg's paper.
#' @return A numeric scalar identifying the chord's periodicity.
#' High values mean a higher period length, lower periodicity,
#' and lower consonance.
#' @references
#'   \insertAllCited{}
#' @rdname smooth_log_periodicity
#' @export
smooth_log_periodicity <- function(x, d = 0.011) {
  UseMethod("smooth_log_periodicity")
}

#' @rdname smooth_log_periodicity
#' @export
smooth_log_periodicity.default <- function(x, d = 0.011) {
  x <- hrep::pi_chord(x)
  do.call(smooth_log_periodicity, as.list(environment()))
}

#' @rdname smooth_log_periodicity
#' @export
smooth_log_periodicity.pi_chord <- function(x, d = 0.011) {
  checkmate::qassert(d, "N1(0,)")
  chord <- as.numeric(x)
  mean(vapply(seq_along(x), function(i) {
    tmp_chord <- x - x[i]
    log2(relative_periodicity(rationalise_chord(tmp_chord, d = d)))
  }, numeric(1)))
}

# See DOI: 10.1080/17459737.2015.1033024
# @param x Number to approximate
# @param d Tolerance ratio
fraction <- function(x, d, verbose = FALSE) {
  x_min <- (1 - d) * x
  x_max <- (1 + d) * x
  a_l <- floor(x)
  b_l <- 1
  a_r <- floor(x) + 1
  b_r <- 1
  a <- round(x)
  b <- 1
  while(a / b < x_min || x_max < a / b) {
    x_0 <- 2 * x - a / b
    if (x < a / b) {
      a_r <- a
      b_r <- b
      k <- floor((x_0 * b_l - a_l) / (a_r - x_0 * b_r))
      a_l <- a_l + k * a_r
      b_l <- b_l + k * b_r
    } else {
      a_l <- a
      b_l <- b
      k <- floor((a_r - x_0 * b_r) / (x_0 * b_l - a_l))
      a_r <- a_r + k * a_l
      b_r <- b_r + k * b_l
    }
    a <- a_l + a_r
    b <- b_l + b_r
    if (verbose) message("a = ", a, ", b = ", b)
  }
  c(a, b)
}

# Uses rational tuning system 2
# Non-integer inputs permitted
# @param d = 0.011 corresponds to rational tuning 2
get_rational_interval <- function(x, d) {
  stopifnot(length(x) == 1L)
  octave <- floor(x / 12)
  pitch_class <- x %% 12
  res <- fraction(2 ^ (pitch_class / 12), d = 0.011)
  while (octave != 0) {
    if (octave < 0) {
      res <- half_fraction(res)
      octave <- octave + 1L
    } else if (octave > 0) {
      res <- double_fraction(res)
      octave <- octave - 1L
    }
  }
  res
}

half_fraction <- function(x) {
  stopifnot(length(x) == 2L)
  if (x[1] %% 2L == 0L) x[1] <- x[1] / 2L else x[2] <- x[2] * 2L
  x
}

double_fraction <- function(x) {
  stopifnot(length(x) == 2L)
  if (x[2] %% 2L == 0L) x[2] <- x[2] / 2L else x[1] <- x[1] * 2L
  x
}

rationalise_chord <- function(x, d) {
  sapply(x, function(y) get_rational_interval(y, d))
}

relative_periodicity <- function(x) {
  stopifnot(is.matrix(x), nrow(x) == 2, ncol(x) > 0L)
  lcm(x[2, ])  * x[1, 1] / x[2, 1]
}

lcm <- function(x) {
  if (length(x) == 1L) x else if (length(x) == 2L) {
    gmp::lcm.default(x[1], x[2])
  } else lcm(c(x[1], lcm(x[-1])))
}
