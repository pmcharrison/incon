#' Spectral roughness (Hutchinson & Knopoff)
#'
#' Gets the roughness of a sonority according to the model of
#' \insertCite{Hutchinson1978;textual}{incon}.
#' @param x Object to analyse, which is coerced to the class
#' \code{\link[hrep]{sparse_fr_spectrum}}.
#' * Numeric vectors will be treated as vectors of MIDI note numbers,
#' and expanded into their implied harmonics.
#' * Two-element lists will be treated as finalised spectra,
#' with the first element being a numeric vector of frequencies,
#' and the second element being a numeric vector of amplitudes.
#'
#' @param a Parameter passed to \code{\link{hutch_g}()}.
#'
#' @param b Parameter passed to \code{\link{hutch_g}()}.
#'
#' @param cbw_cut_off Parameter passed to \code{\link{hutch_g}()}.
#'
#' @param dissonance_function
#' Function for computing dissonance contribution as a function of
#' critical bandwidth distance, defaulting to \code{\link{hutch_dissonance_function}}.
#' Custom functions may be specified here as long as they use the same parameter list
#' as the original function.
#'
#' @return Numeric scalar, identifying the roughness of the spectrum.
#'
#' @references
#' \insertAllCited{}
#'
#' @rdname roughness_hutch
#'
#' @md
#'
#' @export
roughness_hutch <- function(
    x,
    cbw_cut_off = 1.2,
    a = 0.25,
    b = 2,
    dissonance_function = hutch_dissonance_function,
    ...) {
  UseMethod("roughness_hutch")
}

#' @param ... Further arguments to pass to \code{\link[hrep]{sparse_fr_spectrum}}.
#' @rdname roughness_hutch
#' @export
roughness_hutch.default <- function(
    x,
    cbw_cut_off = 1.2,
    a = 0.25,
    b = 2,
    dissonance_function = hutch_dissonance_function,
    ...
) {
  x <- hrep::sparse_fr_spectrum(x, ...)
  roughness_hutch(x, cbw_cut_off = cbw_cut_off, a = a, b = b, dissonance_function = dissonance_function)
}

#' @rdname roughness_hutch
#' @export
roughness_hutch.sparse_fr_spectrum <- function(
    x,
    cbw_cut_off = 1.2,
    a = 0.25,
    b = 2,
    dissonance_function = hutch_dissonance_function,
    ...
) {
  frequency <- hrep::freq(x)
  amplitude <- hrep::amp(x)
  n <- length(frequency)
  if (n < 2) 0 else {
    # Compute denominator
    denominator <- sum(amplitude ^ 2)
    # Compute numerator
    df <- expand.grid(j = seq_len(n), i = seq_len(n)) %>%
      (function(df) {
        df[df$i < df$j, ]
      })
    df$a_i_a_j <- amplitude[df$i] * amplitude[df$j]
    df$g_ij <- dissonance_function(
      f1 = frequency[df$i],
      f2 = frequency[df$j],
      cbw_cut_off = cbw_cut_off,
      a = a,
      b = b
    )
    numerator <- sum(df$a_i_a_j * df$g_ij)
    numerator / denominator
  }
}

#' Get dissonance contribution
#'
#' Computes the dissonance contribution of a pair of pure tones.
#'
#' @inheritParams roughness_hutch
#' @inheritParams hutch_cbw
#'
#' @return Numeric vector of dissonance contributions.
#'
#' @export
hutch_dissonance_function <- function(f1, f2, cbw_cut_off = 1.2, a = 0.25, b = 2) {
  hutch_g(
    y = hutch_y(f1 = f1, f2 = f2),
    cbw_cut_off = cbw_cut_off,
    a = a,
    b = b
  )
}

#' Critical bandwidth (Hutchison & Knopoff)
#'
#' Calculates the critical bandwidth given pairs of frequencies
#' \code{f1} and \code{f2},
#' according to the model of \insertCite{Hutchinson1978;textual}{incon}.
#' @param f1 (Numeric vector) Frequency 1, Hz
#' @param f2 (Numeric vector) Frequency 2, Hz
#' @return (Numeric vector) Critical bandwidths.
#' @references
#'   \insertAllCited{}
#' @export
hutch_cbw <- function(f1, f2) {
  mean_f <- (f1 + f2) / 2
  1.72 * (mean_f ^ 0.65)
}

#' Critical bandwidth distance (Hutchison & Knopoff)
#'
#' Calculates the distance between pairs of frequencies in units of
#' critical bandwidths, according to the model of
#' \insertCite{Hutchinson1978;textual}{incon}.
#' @param f1 (Numeric vector) Frequency 1, Hz
#' @param f2 (Numeric vector) Frequency 2, Hz
#' @return (Numeric vector) Unsigned distances in frequency bandwidths.
#' @references
#'   \insertAllCited{}
#' @export
hutch_y <- function(f1, f2) {
  abs_freq_diff <- abs(f1 - f2)
  critical_bandwidth <- hutch_cbw(f1, f2)
  abs_freq_diff / critical_bandwidth
}

#' Dissonance factor (Hutchinson & Knopoff)
#'
#' Computes dissonance factors given frequency distances in units of
#' critical bandwidths, after Mashinter's implementation of
#' Hutchinson & Knopoff's model
#' \insertCite{Mashinter2006,Hutchinson1978}{incon}.
#' This function corresponds to an approximation of the
#' look-up table in \insertCite{Plomp1965;textual}{incon}.
#' @param y (Numeric vector) Frequency distance in units of critical bandwidths.
#' @param cbw_cut_off (Numeric scalar)
#' If not \code{NULL}, then should be a number
#' corresponding to the variable CBWcutoff in Mashinter's own implementation.
#' If \code{y >= cbw_cut_off}, then the dissonance factor will be approximated as 0.
#' Setting \code{cbw_cut_off} to 1.2 is necessary for replicating Mashinter's results.
#' A cut-off of 1.2 was also used by \insertCite{Bigand1996;textual}{incon}.
#' @param a (Numeric scalar, default = 0.25)
#' Parameter from \insertCite{Mashinter2006;textual}{incon}.
#' @param b (Numeric scalar, default = 2)
#' Parameter from \insertCite{Mashinter2006;textual}{incon}.
#' @return (Numeric vector) Dissonance factors.
#' @references
#'   \insertAllCited{}
#' @export
hutch_g <- function(y, cbw_cut_off = 1.2, a = 0.25, b = 2) {
  assertthat::assert_that(
    is.numeric(y), all(y >= 0),
    is.null(cbw_cut_off) || (assertthat::is.scalar(cbw_cut_off) && is.numeric(cbw_cut_off)),
    is.numeric(a), is.numeric(b),
    assertthat::is.scalar(a), assertthat::is.scalar(b)
  )
  res <- ((y / a) * exp(1 - (y / a))) ^ b
  if (!is.null(cbw_cut_off)) {
    res[y > cbw_cut_off] <- 0
  }
  res
}

hutch_visualise_data <- function(x, cbw_cut_off, a, b, min_freq, max_freq, ...) {
  frequency <- c(min_freq, hrep::freq(x), max_freq)
  amplitude <- c(0, hrep::amp(x), 0)
  n <- length(frequency)
  df <- expand.grid(j = seq_len(n), i = seq_len(n)) %>%
    tibble::as_tibble() %>%
    dplyr::mutate(
      a_i_a_j = amplitude[.data$i] * amplitude[.data$j],
      g_ij = hutch_g(
        y = hutch_y(f1 = frequency[.data$i],
                    f2 = frequency[.data$j]),
        cbw_cut_off = cbw_cut_off,
        a = a,
        b = b
      )) %>%
    dplyr::group_by(.data$i) %>%
    dplyr::summarise(dissonance = sum(.data$g_ij))
  df2 <- tibble::tibble(i = seq_len(n), frequency, amplitude) %>%
    dplyr::left_join(df, by = "i") %>%
    dplyr::mutate(pitch = hrep::freq_to_midi(frequency))
  df3 <- data.frame(pitch = numeric(n * 5),
                    amplitude = numeric(n * 5),
                    dissonance = numeric(n * 5))
  for (i in seq_len(n)) {
    I <- (i - 1L) * 5L
    df3[I + 1:5, "pitch"] <- df2[i, "pitch"]
    df3[I + 2:4, "dissonance"] <- df2[i, "dissonance"]
    df3$amplitude[I + 3L] <- df2$amplitude[i]
  }
  df3
}

#' ggplot theme
#'
#' Defines a default theme for visualising computations
#' for Hutchinson & Knopoff's (1978) model
#' (see \code{\link{hutch_visualise}}).
#' @export
hutch_visualise_theme <- ggplot2::theme_classic() +
  ggplot2::theme(
    panel.spacing = ggplot2::unit(1.9, "lines"),
    strip.background = ggplot2::element_blank(),
    axis.text.x = ggplot2::element_text(colour = "black"),
    axis.text.y = ggplot2::element_text(colour = "black"),
    axis.ticks = ggplot2::element_line(colour = "black")
  )

#' Visualise
#'
#' Creates a plot visualising computations for Hutchinson & Knopoff's model.
#'
#' @param x Passed to \code{\link{roughness_hutch}}.
#' @param cbw_cut_off Passed to \code{\link{roughness_hutch}}.
#' @param a Passed to \code{\link{roughness_hutch}}.
#' @param b Passed to \code{\link{roughness_hutch}}.
#' @param label (Character scalar) x-axis label.
#' @param amplitude_breaks Numeric vector of tick locations for the y-axis.
#' @param colour_limits Defines the limits of the roughness scale.
#' @param colour_low Colour to use for the lowest roughness.
#' @param colour_high Colour to use for the highest roughness.
#' @param theme \code{\link[ggplot2]{ggplot}} theme to use.
#' @param ... Passed to \code{\link[hrep]{sparse_fr_spectrum}}.
#' @export
hutch_visualise <- function(x,
                            cbw_cut_off = 1.2,
                            a = 0.25,
                            b = 2,
                            label = "Roughness",
                            amplitude_breaks = c(0, 1),
                            colour_limits = c(0, 3),
                            colour_low = "darkblue",
                            colour_high = "red",
                            theme = hutch_visualise_theme,
                            ...) {
  stopifnot(is.list(x), !is.null(names(x)), !anyDuplicated(names(x)))
  x <- purrr::map(x, hrep::sparse_fr_spectrum, ...)
  min_freq <- min(purrr::map_dbl(x, ~ min(hrep::freq(.))))
  max_freq <- max(purrr::map_dbl(x, ~ max(hrep::freq(.))))
  labels <- factor(names(x), levels = names(x))
  purrr::map(x, hutch_visualise_data, cbw_cut_off, a, b, min_freq, max_freq, ...) %>%
    purrr::map2(labels, ~ dplyr::mutate(.x, label = .y)) %>%
    dplyr::bind_rows() %>%
    ggplot2::ggplot(ggplot2::aes_string(x = "pitch",
                                        y = "amplitude",
                                        colour = "dissonance")) +
    ggplot2::geom_line() +
    ggplot2::scale_x_continuous("Pitch (MIDI)") +
    ggplot2::scale_y_continuous("Amplitude", breaks = amplitude_breaks) +
    ggplot2::scale_colour_gradient(label,
                                   low = colour_low,
                                   high = colour_high,
                                   limits = colour_limits) +
    ggplot2::facet_wrap(~ label, ncol = 1) +
    theme
}

#' Spectral roughness (Sethares)
#'
#' Gets the roughness of a sonority according to the model of Sethares (1993).
#' By default, the algorithm is modified according to
#' \insertCite{Sethares2005;textual}{incon} and
#' \insertCite{Weisser2013;textual}{incon}:
#' roughness is proportional to the minimum amplitude of each pair of partials,
#' not the product of their amplitudes.
#' This behaviour can be disabled by setting \code{min_amplitude = FALSE}.
#' @param x Object to analyse, which is coerced to the class
#' \code{\link[hrep]{sparse_fr_spectrum}}.
#' * Numeric vectors will be treated as vectors of MIDI note numbers,
#' and expanded into their implied harmonics.
#' * Two-element lists will be treated as finalised spectra,
#' with the first element being a numeric vector of frequencies,
#' and the second element being a numeric vector of amplitudes.
#' @param min_amplitude See \code{\link{dyad_roughness_seth}}.
#' @return Estimated roughness, as a numeric scalar.
#' @note \insertCite{Sethares2005;textual}{incon}
#' suggests using loudnesses instead of amplitudes.
#' However, he acknowledges that loudness is difficult to calculate
#' for arbitrary timbres.
#' Furthermore, if we replace amplitude with roughness,
#' we lose the original model's invariance to multiplicative
#' scaling of the original signal.
#' In this implementation, we therefore stay with amplitude,
#' consistent with \insertCite{Sethares1993;textual}{incon}.
#' @references
#' \insertAllCited{}
#' @rdname roughness_seth
#' @md
#' @export
roughness_seth <- function(x, min_amplitude = TRUE, ...) {
  UseMethod("roughness_seth")
}

#' @param ... Further arguments to pass to \code{\link[hrep]{sparse_fr_spectrum}}.
#' @rdname roughness_seth
#' @export
roughness_seth.default <- function(x, min_amplitude = TRUE, ...) {
  x <- hrep::sparse_fr_spectrum(x, ...)
  roughness_seth(x, min_amplitude = min_amplitude)
}

#' @rdname roughness_seth
#' @export
roughness_seth.sparse_fr_spectrum <- function(x, min_amplitude = TRUE, ...) {
  frequency <- hrep::freq(x)
  amplitude <- hrep::amp(x)
  n <- length(frequency)
  if (n < 2) 0 else {
    # The formula given in Sethares (1993) iterates over all pairs of [i, j],
    # but because roughness is symmetric we can save time by only considering pairs.
    # This gets rid of the 'divide by two' component in the original equation.
    df <- expand.grid(j = seq_len(n), i = seq_len(n)) %>%
      (function(df) {
        df[df$i < df$j, ]
      })
    dyad_roughness_seth(
      f1 = frequency[df$i],
      f2 = frequency[df$j],
      a1 = amplitude[df$i],
      a2 = amplitude[df$j],
      min_amplitude = min_amplitude
    ) %>% sum
  }
}

#' Dyad roughness (Sethares)
#'
#' Gets the roughness of a dyad according to the model of Sethares (1993).
#' @param f1 Frequency of tone 1 (Hz) (numeric vector).
#' @param f2 Frequency of tone 2 (Hz) (numeric vector). Must be greater than \code{f1}.
#' @param a1 amplitude of tone 1 (numeric vector).
#' @param a2 amplitude of tone 2 (numeric vector).
#' @param ensure_f1_is_less_than_f2 If \code{FALSE}, assumes that \code{f1 < f2}.
#' @param min_amplitude If \code{TRUE},
#' roughness is considered to be proportional to
#' the minimum amplitude of each pair of partials,
#' rather than the product of their amplitudes.
#' The default (\code{TRUE}) corresponds to the algorithm as updated by
#' \insertCite{Sethares2005;textual}{incon} and
#' \insertCite{Weisser2013;textual}{incon}.
#' Set to \code{FALSE} to recover the original algorithm from
#' \insertCite{Sethares1993;textual}{incon}.
#' @param a Numeric scalar parameter, optimised to 3.5 (default) in Sethares (1993).
#' @param b Numeric scalar parameter, optimised to 5.75 (default) in Sethares (1993).
#' @param s1 Numeric scalar parameter from Sethares (1993).
#' @param s2 Numeric scalar parameter from Sethares (1993).
#' @param d_star Numeric scalar parameter from Sethares (1993).
#' @return Numeric vector of roughnesses.
#' @references
#' \insertAllCited{}
dyad_roughness_seth <- function(f1, f2, a1, a2,
                                ensure_f1_is_less_than_f2 = TRUE,
                                min_amplitude = TRUE,
                                a = 3.5,
                                b = 5.75,
                                s1 = 0.021,
                                s2 = 19,
                                d_star = 0.24) {
  assertthat::assert_that(
    is.numeric(f1), is.numeric(f2),
    is.numeric(a1), is.numeric(a2),
    is.logical(min_amplitude), length(min_amplitude) == 1L,
    length(f1) == length(f2),
    length(f1) == length(a1),
    length(f1) == length(a2)
  )
  if (ensure_f1_is_less_than_f2) {
    need_reversal <- f1 > f2
    dyad_roughness_seth(
      f1 = ifelse(need_reversal, f2, f1),
      f2 = ifelse(need_reversal, f1, f2),
      a1 = ifelse(need_reversal, a2, a1),
      a2 = ifelse(need_reversal, a1, a2),
      ensure_f1_is_less_than_f2 = FALSE,
      a = a, b = b, s1 = s1, s2 = s2, d_star = d_star
    )
  } else {
    s <- d_star / (s1 * f1 + s2)
    A <- if (min_amplitude) pmin(a1, a2) else a1 * a2
    A * (
      exp(- a * s * (f2 - f1)) -
        exp(- b * s * (f2 - f1))
    )
  }
}

#' Spectral roughness (Vassilakis)
#'
#' Gets the roughness of a sonority according to the model of
#' \insertCite{Vassilakis2001;textual}{incon}
#' \insertCite{Villegas2010;textual}{incon}
#' @param x Object to analyse, which is coerced to the class
#' \code{\link[hrep]{sparse_fr_spectrum}}.
#' * Numeric vectors will be treated as vectors of MIDI note numbers,
#' and expanded into their implied harmonics.
#' * Two-element lists will be treated as finalised spectra,
#' with the first element being a numeric vector of frequencies,
#' and the second element being a numeric vector of amplitudes.
#' @return Estimated roughness, as a numeric scalar.
#' @references
#' \insertAllCited{}
#' @rdname roughness_vass
#' @md
#' @export
roughness_vass <- function(x, ...) {
  UseMethod("roughness_vass")
}

#' @param ... Further arguments to pass to \code{\link[hrep]{sparse_fr_spectrum}}.
#' @rdname roughness_vass
#' @export
roughness_vass.default <- function(x, ...) {
  x <- hrep::sparse_fr_spectrum(x, ...)
  roughness_vass(x)
}

#' @rdname roughness_vass
#' @export
roughness_vass.sparse_fr_spectrum <- function(x, ...) {
  tolerance <- 1e-5
  x <- x[x$y > tolerance, ] # eliminate partials with near-zero weight
  frequency <- hrep::freq(x)
  amplitude <- hrep::amp(x)
  n <- length(frequency)
  if (n < 2) 0 else {
    # Roughness is computed by summing over all dyadic roughnesses.
    # Noting that the formula for dyadic roughness is symmetric,
    # we can instead only compute dyadic roughnesses for pairs
    # where i < j, and then double the resulting sum.
    df <- expand.grid(j = seq_len(n), i = seq_len(n)) %>%
      (function(df) {
        df[df$i < df$j, ]
      })
    dyad_roughness_vass(
      f1 = frequency[df$i],
      f2 = frequency[df$j],
      a1 = amplitude[df$i],
      a2 = amplitude[df$j]
    ) %>% sum %>% magrittr::multiply_by(2)
  }
}

#' Dyad roughness (Vassilakis)
#'
#' Gets the roughness of a dyad according to the model of
#' \insertCite{Vassilakis2001;textual}{incon}
#' \insertCite{Villegas2010;textual}{incon}
#' @param f1 Frequency of tone 1 (Hz) (numeric vector).
#' @param f2 Frequency of tone 2 (Hz) (numeric vector).
#' @param a1 amplitude of tone 1 (numeric vector).
#' @param a2 amplitude of tone 2 (numeric vector).
#' @return Numeric vector of roughnesses.
#' @note The function is vectorised over all inputs.
#' @references
#' \insertAllCited{}
dyad_roughness_vass <- function(f1, f2, a1, a2) {
  assertthat::assert_that(
    is.numeric(f1), is.numeric(f2),
    is.numeric(a1), is.numeric(a2),
    length(f1) == length(f2),
    length(f1) == length(a1),
    length(f1) == length(a2)
  )
  ((a1 * a2) ^ 0.1) *
    0.5 *
    (((2 * pmin(a1, a2)) / (a1 + a2)) ^ 3.11) *
    (exp(- 3.5 * f_vass(f1, f2)) - exp(- 5.75 * f_vass(f1, f2)))
}

f_vass <- function(f1, f2) {
  s_vass(pmin(f1, f2)) *
    abs(f1 - f2)
}

s_vass <- function(f) {
  0.24 / (0.0207 * f + 18.96)
}
