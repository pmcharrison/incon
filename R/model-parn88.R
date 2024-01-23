#' Root by pitch-class chord
#'
#' This vector stores precomputed chord roots for
#' all pitch-class chords,
#' indexed by their encoding as defined in the "hrep" package.
#' See \code{\link[hrep]{pc_chord}} for more details.
#'
#' @name root_by_pc_chord
#' @docType data
#' @keywords data
NULL

#' Parncutt (1988)
#'
#' Analyses a pitch-class set using the root-finding model of
#' \insertCite{Parncutt1988;textual}{parn88}.
#' @param x Sonority to analyse.
#' This will be coerced to an object of class \code{\link[hrep]{pc_set}}.
#' @param root_support (Character scalar or data frame)
#' Identifies the root support weights to use.
#' * \code{"v2"} (default) uses the updated
#' weights from \insertCite{Parncutt2006;textual}{parn88}.
#' * \code{"v1"} uses the original weights from \insertCite{Parncutt2006;textual}{parn88}.
#'
#' See \code{\link{root_support_weights}} for the values of these weights.
#' Alternatively, root-support weights can be provided as a data frame,
#' with one column (interval) identifying the ascending interval in semitones,
#' and another column (weight) identifying the corresponding root support weight.
#' @param exponent (Numeric scalar) Exponent to be used when computing
#' root ambiguities. Defaults to 0.5, after \insertCite{Parncutt1988;textual}{parn88}.
#' @return A list with three values:
#' * \code{root}, the estimated chord root (integer scalar);
#' * \code{root_ambiguity}, the root ambiguity (numeric scalar),
#' * \code{pc_weight}, a 12-dimensional vector of weights by pitch class.
#' @references
#'   \insertAllCited{}
#' @md
#' @rdname parn88
#' @export
parn88 <- function(x, root_support = "v2", exponent = 0.5) {
  UseMethod("parn88")
}

#' @rdname parn88
#' @export
parn88.default <- function(x, root_support = "v2", exponent = 0.5) {
  x <- hrep::pc_set(x)
  do.call(parn88, args = as.list(environment()))
}

#' @rdname parn88
#' @export
parn88.pc_set <- function(x, root_support = "v2", exponent = 0.5) {
  root_support <- get_root_support_weights(root_support)

  checkmate::qassert(x, "X+[0,11]")
  checkmate::qassert(exponent, "R1")
  checkmate::qassert(root_support, "R12")
  stopifnot(!anyDuplicated(x))

  w <- purrr::map_dbl(0:11,
                      pc_weight,
                      pc_set = encode_pc_set(x),
                      root_support = root_support)

  list(root = which.max(w) - 1L,
       root_ambiguity = get_root_ambiguity(w, exponent = exponent),
       pc_weight = w)
}

#' Root
#'
#' Estimates the chord root of a pitch-class set using the root-finding model of
#' \insertCite{Parncutt1988;textual}{parn88}.
#' This function is a wrapper for \code{\link{parn88}}.
#' @param ... Arguments to pass to \code{\link{parn88}}.
#' @return The estimated chord root (integer scalar).
#' @references
#'   \insertAllCited{}
#' @export
root <- function(...) {
  parn88(...)$root
}

#' Root ambiguity
#'
#' Estimates the root ambiguity of a pitch-class set using the root-finding model of
#' \insertCite{Parncutt1988;textual}{parn88}.
#' This function is a wrapper for \code{\link{parn88}}.
#' @param ... Arguments to pass to \code{\link{parn88}}.
#' @return The root ambiguity (numeric scalar).
#' @references
#'   \insertAllCited{}
#' @export
root_ambiguity <- function(...) {
  parn88(...)$root_ambiguity
}

#' Root support weights
#'
#' A list of different root support weights that may be used
#' by the root-finding algorithm of \insertCite{Parncutt1988;textual}{parn88}.
#' See \code{\link{parn88}} for more information.
#' @references
#'   \insertAllCited{}
#' @export
root_support_weights <- list(
  v1 = tibble::tribble(
    ~ interval, ~ weight,
    0, 1,
    7, 1/2,
    4, 1/3,
    10, 1/4,
    2, 1/5,
    3, 1/10
  ),
  v2 = tibble::tribble(
    ~ interval, ~ weight,
    0, 10,
    7, 5,
    4, 3,
    10, 2,
    2, 1
  )
) %>% purrr::map(function(df) {
  x <- numeric(12)
  x[df$interval + 1] <- df$weight
  x
})

get_root_ambiguity <- function(x, exponent) {
  checkmate::qassert(x, "R12")
  checkmate::qassert(exponent, "R1")
  x_max <- max(x)
  sum(x / x_max) ^ exponent
}

pc_weight <- function(pc, pc_set, root_support) {
  checkmate::qassert(pc, "X1")
  checkmate::qassert(pc_set, "X12[0,1]")
  checkmate::qassert(root_support, "R12")
  ind <- (seq(from = pc, length.out = 12L) %% 12L) + 1L
  sum(pc_set[ind] * root_support)
}

encode_pc_set <- function(x) {
  checkmate::qassert(x, "X[0,11]")
  y <- integer(12)
  y[x + 1] <- 1L
  y
}

get_root_support_weights <- function(root_support) {
  if (is.character(root_support)) {
    stopifnot(length(root_support) == 1L,
              root_support %in% names(root_support_weights))
    root_support <- root_support_weights[[root_support]]
  }
}
