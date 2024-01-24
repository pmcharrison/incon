#' Dyadic consonance (Huron 1994)
#'
#' Computes the aggregate dyadic consonance of a chord following
#' Huron (1994).
#'
#' @param x Chord to evaluate, will be coerced to a
#' pitch-class set (\code{\link[hrep]{pc_set}}).
#' @export
huron_1994 <- function(x) {
  UseMethod("huron_1994")
}

#' @export
huron_1994.default <- function(x) {
  x <- hrep::pc_set(x)
  huron_1994.pc_set(x)
}

#' @export
huron_1994.pc_set <- function(x) {
  x <- hrep::int_vec(x)
  huron_1994.int_vec(x)
}

#' @export
huron_1994.int_vec <- function(x) {
  sum(x * huron_1994_weights)
}

#' Dyadic consonance weights (Huron 1994)
#'
#' A vector of the weights used in Huron's (1994) dyadic consonance model.
#' @export
huron_1994_weights <- c(- 1.428,
                        - 0.582,
                        0.594,
                        0.386,
                        1.240,
                        -0.453)
