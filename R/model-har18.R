#' Cosine similarity
#'
#' Computes the cosine similarity between two numeric vectors.
#' @param x Numeric vector 1.
#' @param y Numeric vector 2.
#' @return Cosine similarity, as a numeric scalar.
#' @export
cosine_similarity <- function(x, y) {
  numerator <- sum(x * y)
  denominator <-
    sqrt(sum(x ^ 2)) *
    sqrt(sum(y ^ 2))
  numerator / denominator
}

#' Peak
#'
#' Gets the peak value of an object.
#' @param x Object to analyse.
#' @return The object's peak value, as a numeric scalar.
#' @rdname peak
#' @export
peak <- function(x) {
  UseMethod("peak")
}

#' @rdname peak
#' @export
peak.milne_pc_spectrum <- function(x) {
  max(as.numeric(x))
}

#' Kullback-Leibler divergence from uniform
#'
#' Gets the Kullback-Leibler divergence of a provided distribution
#' from a uniform distribution.
#' @param x Input distribution.
#' @return The Kullback-Leibler divergence from a uniform distribution
#' to the input distribution.
#' @rdname kl_div_from_uniform
#' @export
kl_div_from_uniform <- function(x) {
  UseMethod("kl_div_from_uniform")
}

#' @rdname kl_div_from_uniform
#' @export
kl_div_from_uniform.smooth_spectrum <- function(x) {
  # Construct a probability vector, where each bin corresponds to
  # the probability of a discrete event
  x <- as.numeric(x)
  probs <- x / sum(x)
  n <- length(probs)
  uniform_probs <- 1 / n
  non_zero_probs <- probs[probs > 0]
  sum(
    non_zero_probs * log(non_zero_probs / uniform_probs, base = 2)
  )
}

#' Pitch-class harmonicity
#'
#' Gets the pitch-class harmonicity of an input sonority, after
#' \insertCite{Harrison2018;textual}{incon} and
#' \insertCite{Milne2013;textual}{incon}.
#' @param x Object to analyse.
#' @param method (Character scalar) Method to use.
#' * \code{"kl"} (default) delivers the Kullback-Leibler method of
#' \insertCite{Harrison2018;textual}{incon}.
#' * \code{"peak"} delivers the peak-value method of
#' \insertCite{Milne2013;textual}{incon}.
#' @param num_harmonics (Integerish scalar)
#' Number of harmonics to use when expanding tones into their implied harmonics,
#' and when defining the harmonic template
#' (including the fundamental frequency).
#' Defaults to 12, after
#' \insertCite{Milne2016;textual}{incon}.
#' @param rho (Numeric scalar)
#' Roll-off parameter for harmonic expansion.
#' Defaults to 0.75, after
#' \insertCite{Milne2016;textual}{incon}.
#' @param sigma (Numeric scalar)
#' Standard deviation of the Gaussian smoothing distribution (cents).
#' Defaults to 6.83, after
#' \insertCite{Milne2016;textual}{incon}.
#' @param array_dim (Integerish scalar)
#' Dimensionality of the pitch-class spectrum array.
#' Defaults to 1200, after
#' \insertCite{Milne2016;textual}{incon}.
#' @param ... Arguments passed to specific methods.
#' @return Pitch-class harmonicity, as a numeric scalar.
#' @note This algorithm makes use of \code{\link[hrep]{milne_pc_spectrum}()}
#' as defined in the \code{hrep} package.
#' @md
#' @references
#' \insertAllCited{}
#' @examples
#' pc_harmonicity(c(0, 4, 7))
#' pc_harmonicity(c(0, 3, 7))
#' pc_harmonicity(c(0, 3, 6))
#' @rdname pc_harmonicity
#' @export
pc_harmonicity <- function(x,
                           method = "kl",
                           num_harmonics = 12,
                           rho = 0.75,
                           sigma = 6.83,
                           ...) {
  UseMethod("pc_harmonicity")
}

#' @rdname pc_harmonicity
#' @export
pc_harmonicity.default <- function(x,
                                   method = "kl",
                                   num_harmonics = 12,
                                   rho = 0.75,
                                   sigma = 6.83,
                                   array_dim = 1200,
                                   ...) {
  x <- hrep::pc_set(x)
  do.call(pc_harmonicity, as.list(environment()))
}

#' @rdname pc_harmonicity
#' @export
pc_harmonicity.pc_set <- function(x,
                                  method = "kl",
                                  num_harmonics = 12,
                                  rho = 0.75,
                                  sigma = 6.83,
                                  array_dim = 1200,
                                  ...) {
  checkmate::qassert(method, "S1")

  x <- hrep::milne_pc_spectrum(x,
                               num_harmonics = num_harmonics,
                               rho = rho,
                               sigma = sigma,
                               array_dim = array_dim)
  pc_harmonicity(x,
                 method = method,
                 num_harmonics = num_harmonics,
                 rho = rho,
                 sigma = sigma)
}

#' @rdname pc_harmonicity
#' @export
pc_harmonicity.milne_pc_spectrum <- function(x,
                                             method = "kl",
                                             num_harmonics = 12,
                                             rho = 0.75,
                                             sigma = 6.83,
                                             ...) {
  checkmate::qassert(method, "S1")

  y <- sweep_harmonic_template(x,
                               num_harmonics = num_harmonics,
                               rho = rho,
                               sigma = sigma)
  if (method == "kl") {
    kl_div_from_uniform(y)
  } else if (method == "peak") {
    peak(y)
  } else stop("unrecognised method")
}

#' Sweep harmonic template
#'
#' Sweeps a harmonic template over an input spectrum.
#' @param x Object to analyse.
#' @param num_harmonics See \code{\link{pc_harmonicity}}.
#' @param rho See \code{\link{pc_harmonicity}}.
#' @param sigma See \code{\link{pc_harmonicity}}.
#' @param array_dim See \code{\link{pc_harmonicity}}.
#' @param ... Arguments passed to specific methods.
#' @return An object of class \code{\link[hrep]{milne_pc_spectrum}},
#' identifying each pitch class with a perceptual weight
#' corresponding to its harmonic template fit.
#' @rdname sweep_harmonic_template
#' @export
sweep_harmonic_template <- function(x,
                                    num_harmonics = 12,
                                    rho = 0.75,
                                    sigma = 6.83,
                                    array_dim = 1200,
                                    ...) {
  UseMethod("sweep_harmonic_template")
}

#' @rdname sweep_harmonic_template
#' @export
sweep_harmonic_template.pc_set <- function(x,
                                           num_harmonics = 12,
                                           rho = 0.75,
                                           sigma = 6.83,
                                           array_dim = 1200,
                                           ...) {
  hrep::milne_pc_spectrum(x,
                          num_harmonics = num_harmonics,
                          rho = rho,
                          sigma = sigma,
                          array_dim = array_dim) %>%
    sweep_harmonic_template(num_harmonics = num_harmonics,
                            rho = rho,
                            sigma = sigma)
}

#' @rdname sweep_harmonic_template
#' @export
sweep_harmonic_template.milne_pc_spectrum <- function(x,
                                                      num_harmonics = 12,
                                                      rho = 0.75,
                                                      sigma = 6.83,
                                                      ...) {
  x <- as.numeric(x)
  array_dim <- length(x)
  template <- hrep::milne_pc_spectrum(hrep::pc_set(0),
                                      array_dim = array_dim,
                                      num_harmonics = num_harmonics,
                                      rho = rho,
                                      sigma = sigma)
  res <- sweep_template(x, template)

  hrep::.milne_pc_spectrum(res)
}

#' Sweep template
#'
#' Sweeps a circular template over a circular vector
#' and computes the cosine similarity at each possible offset.
#'
#' @param x
#' (Numeric vector)
#' The vector to be swept over.
#'
#' @param template
#' (Numeric vector)
#' The template to sweep over \code{x}.
#' Should have the same dimensionality as \code{x}.
#'
#' @param legacy
#' (Logical scalar)
#' Whether to use the legacy R implementation
#' (default = \code{FALSE}).
#' Otherwise the faster C++ implementation is used.
#'
#' @export
sweep_template <- function(x, template, legacy = FALSE) {
  if (!legacy) {
    return(sweep_template_cpp(x, template))
  }

  array_dim <- length(x)
  res <- numeric(array_dim)

  for (i in seq_len(array_dim)) {
    indices <- 1 + (seq(from = i - 1, length.out = array_dim) %% array_dim)
    res[i] <- cosine_similarity(template, x[indices])
  }

  res
}
