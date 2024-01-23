#' Regression coefficients
#'
#' Tabulates regression coefficients for Harrison & Pearce's (in preparation)
#' composite consonance model.
#' @docType data
#' @keywords data
#' @export
har_19_composite_coef <- tibble::tribble(
  ~ term,               ~ coef,
  "intercept",          0.628434666589357,
  "chord_size",         0.422267698605598,
  "hutch_78_roughness", -1.62001025973261,
  "har_18_harmonicity", 1.77992362857478,
  "har_19_corpus",      -0.0892234643584134
)

#' Composite consonance model
#'
#' This function computes the consonance of a chord
#' according to Harrison & Pearce's (in preparation)
#' composite consonance model.
#'
#' The model combines several sub-models:
#' - `hutch_78_roughness`,
#' the roughness model of \insertCite{Hutchinson1978;textual}{dycon}
#' (see \code{dycon::\link[dycon]{roughness_hutch}});
#' - `har_18_harmonicity`,
#' the harmonicity model of \insertCite{Harrison2018;textual}{har18}
#' (see \code{har18::\link[har18]{pc_harmonicity}});
#' - `har_19_corpus`:
#' a corpus-based model of cultural familiarity
#' (Harrison & Pearce, in preparation)
#' (see \code{incon::\link[incon]{corpus_dissonance}}).
#'
#' This model uses the regression coefficients
#' provided in \code{\link{har_19_composite_coef}}, with one caveat:
#' by default, the chord size effect is disabled,
#' because it's thought that this effect came from a confound
#' in the perceptual data of \insertCite{Bowling2018;textual}{incon}.
#'
#' @param x Chord to analyse; passed to \code{\link{incon}}.
#' All chord pitches must be integer-valued.
#' @param chord_size (Boolean) Whether to include the chord size effect
#' (see \strong{Details}).
#' @param num_harmonics (Integerish scalar)
#' Number of harmonics in each tone; passed to \code{\link{incon}}.
#' @param roll_off (Numeric scalar)
#' Harmonic roll-off rate; passed to \code{\link{incon}}.
#'
#' @return Estimated consonance, as a numeric scalar.
#'
#' @references
#' \insertAllCited{}
#'
#' @md
har_19_composite <- function(x, chord_size = FALSE,
                            num_harmonics = 11L, roll_off = 1) {
  checkmate::qassert(chord_size, "B1")
  features <- c(
    intercept = 1,
    chord_size = if (chord_size) length(x) else 0L,
    incon(x,
          model = c("hutch_78_roughness",
                    "har_18_harmonicity",
                    "har_19_corpus"),
          num_harmonics = num_harmonics,
          roll_off = roll_off)
  )
  stopifnot(all(har_19_composite_coef$term == names(features)))
  sum(har_19_composite_coef$coef * features)
}
