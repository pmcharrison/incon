#' Tonal dissonance
#'
#' Computes tonal dissonance using the algorithm
#' of \insertCite{Johnson-Laird2012;textual}{incon}.
#' @param x Sonority to analyse.
#' This will be coerced to an object of class \code{\link[hrep]{pc_set}}.
#' @return Integer scalar identifying the chord's consonance rank,
#' with higher values corresponding to increasing degrees of dissonance.
#' @references
#' \insertAllCited{}
#' @rdname jl_tonal_dissonance
#' @examples
#' jl_tonal_dissonance(c(0, 4, 7))
#' jl_tonal_dissonance(c(0, 3, 7))
#' jl_tonal_dissonance(c(0, 3, 6))
#' @export
jl_tonal_dissonance <- function(x) {
  UseMethod("jl_tonal_dissonance")
}

#' @rdname jl_tonal_dissonance
#' @export
jl_tonal_dissonance.default <- function(x) {
  x <- hrep::pc_set(x)
  jl_tonal_dissonance(x)
}

#' @rdname jl_tonal_dissonance
#' @export
jl_tonal_dissonance.pc_set <- function(x) {
  y <- jl_rule_1(x) * 4L +
    jl_rule_2(x) * 2L +
    jl_rule_3(x) - 3L
  attributes(y) <- NULL
  y
}

#' Tonal Dissonance, Rule 1
#'
#' "Chords occurring in a major scale should be less dissonant
#' than chords occurring only in a minor scale,
#' which in turn should be less dissonant than chords
#' occurring in neither sort of scale."
#' \insertCite{Johnson-Laird2012}{incon}.
#' @param pc_set (Numeric vector) Pitch-class set to analyse.
#' No input checking is performed.
#' @return 1, 2, or 3, corresponding to increasing degrees of dissonance.
#' @references
#' \insertAllCited{}
#' @export
jl_rule_1 <- function(pc_set) {
  if (in_major_scale(pc_set)) 1L else
    if (in_minor_scale(pc_set)) 2L else
      3L
}

#' Tonal Dissonance, Rule 2
#'
#' "Chords that are consistent with a major triad are more consonant
#' than chords that are not consistent with a major triad"
#' \insertCite{Johnson-Laird2012}{incon}.
#' Consistency with the major triad means that a major triad
#' must be contained within the chord.
#' Additionally, all notes must be contained within a major scale.
#' @param pc_set (Numeric vector) Pitch-class set to analyse.
#' No input checking is performed.
#' @return \code{TRUE} for dissonant, \code{FALSE} for consonant.
#' @references
#' \insertAllCited{}
#' @export
jl_rule_2 <- function(pc_set) {
  pc_set <- as.numeric(pc_set)
  for (root in 0:11) {
    triad <- sort((c(0, 4, 7)  + root) %% 12)
    if (all(triad %in% pc_set) &&
        in_major_scale(pc_set)) return(FALSE)
  }
  TRUE
}

#' Tonal Dissonance, Rule 3
#'
#' "Chords built from intervals of a third should be more consonant
#' than chords that are not built from thirds."
#' "The principle allows for just one missing third intervening
#' between two pitch classes a fifth apart"
#' \insertCite{Johnson-Laird2012}{incon}.
#' @param pc_set (Numeric vector) Pitch-class set to analyse.
#' @return \code{FALSE} for consonant, \code{TRUE} for dissonant.
#' If a consonant solution is found,
#' the stacked-thirds version of the chord is returned
#' as the attribute \code{"solution"},
#' accessible with the command \code{attr(x, "solution")}.
#' @references
#' \insertAllCited{}
#' @export
jl_rule_3 <- function(pc_set) {
  N <- length(pc_set)
  pc_set <- as.numeric(pc_set)
  # These contain all the different possible intervallic structures
  # for creating a pitch-class set the same size as the
  # current pitch-class set by stacking thirds, and
  # allowing one pitch class to be missing.
  # Solutions with no pitch class missing correspond to
  # subsets of these vectors.
  perm <- gtools::permutations(n = 2L, r = N,
                               v = c(3L, 4L),
                               repeats.allowed = TRUE)
  # Iterate over every pitch class and try and
  # build the chord by stacking thirds on top of
  # that pitch class
  for (i in seq_along(pc_set)) {
    base <- pc_set[i]
    for (j in seq_len(nrow(perm))) {
      res <- (base + c(0L, cumsum(perm[j, ]))) %% 12L
      if (all(pc_set %in% res)) {
        out <- FALSE
        attr(out, "solution") <- res
        return(out)
      }
    }
  }
  TRUE
}

in_major_scale <- function(pc_set) {
  for (scale in major_scales) {
    if (all(as.numeric(pc_set) %in% scale)) return(TRUE)
  }
  FALSE
}
in_minor_scale <- function(pc_set) {
  for (scale in minor_scales) {
    if (all(as.numeric(pc_set) %in% scale)) return(TRUE)
  }
  FALSE
}

c_major_scale <- c(0, 2, 4, 5, 7, 9, 11)
c_minor_scale <- c(0, 2, 3, 5, 7, 8, 11)

major_scales <- lapply(0:11, function(x) {
  sort((c_major_scale + x) %% 12)
})
minor_scales <- lapply(0:11, function(x) {
  sort((c_minor_scale + x) %% 12)
})
names(major_scales) <- 0:11
names(minor_scales) <- 0:11
