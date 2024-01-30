#' Consonance tables for popular music
#'
#' This table summarises chord prevalences in the
#' McGill Billboard corpus \insertCite{Burgoyne2011}{hcorp}.
#' These pieces were sampled from the Billboard magazine's
#' United States "Hot 100" chart between 1958 and 1991,
#' and transcribed by expert musicians.
#' See \code{\link[hcorp]{popular_1}} for more details.
#'
#' @details
#' Chords are represented as pitch-class chord types:
#' see \code{\link[hrep]{pc_chord_type}} for details.
#'
#' @name popular_1_pc_chord_type
#' @docType data
#' @references
#' \insertAllCited{}
#' @keywords data
NULL

#' Corpus dissonance table
#'
#' Derives sufficient information from a corpus to formulate
#' a corpus-based dissonance model.
#' @param x Corpus to analyse, as created by \code{hrep::\link[hrep]{corpus}}.
#' @param type (Scalar character) Representation to which chords should be coerced
#' before counting.
#' @param add (Scalar numeric) Number to add to each count before computing probabilities.
#' This is useful for ensuring that chord probabilities exceed zero.
#' @return Returns an object of class \code{corpus_dissonance_table},
#' a \link[tibble]{tibble} where each row corresponds to a
#' different pitch-class chord type (i.e. an object of class \code{pc_chord_type}),
#' with the mapping between integer indices and chord types defined by
#' the \code{hrep} package.
#' This tibble contains the following columns:
#' \item{count}{The number of times the chord type was observed in the corpus.}
#' \item{prob}{The inferred probability of that chord type.}
#' \item{log_prob}{The inferred log probability of that chord type.}
#' @rdname corpus_dissonance_table
#' @export
corpus_dissonance_table <- function(x, type = "pc_chord_type", add = 1L) {
  UseMethod("corpus_dissonance_table")
}

#' @rdname corpus_dissonance_table
#' @export
corpus_dissonance_table.corpus <- function(x, type = "pc_chord_type", add = 1L) {
  df <- tibble::tibble(count = count_chords(x, type))
  df$count <- df$count
  df$prob <- (df$count + add) / sum(df$count + add)
  df$neg_log_prob <- - log(df$prob)
  .corpus_dissonance_table(df, type)
}

.corpus_dissonance_table <- function(df, type) {
  class(df) <- c("corpus_dissonance_table", class(df))
  attr(df, "type") <- type
  df
}

#' Get type
#'
#' Gets the type of an object.
#' @param x Object.
#' @return (Character scalar) Type.
#' @export
type <- function(x) {
  UseMethod("type")
}

#' @export
print.corpus_dissonance_table <- function(x, ...) {
  cat("# An object of class 'corpus_dissonance_table'\n")
  cat("# Type: ", type(x), "\n", sep = "")
  NextMethod()
}

#' @export
type.corpus_dissonance_table <- function(x) {
  attr(x, "type")
}

`type<-.corpus_dissonance_table` <- function(x, value) {
  attr(x, "type") <- value
  x
}

#' Corpus dissonance
#'
#' Calculates a corpus-based estimate of the dissonance of a sonority.
#' @details
#' By default, dissonance is estimated from chord prevalences
#' in the McGill Billboard dataset \insertCite{Burgoyne2011}{hcorp}.
#' The dataset's contents were sampled from the Billboard magazine's
#' United States "Hot 100" chart between 1958 and 1991,
#' and transcribed by expert musicians.
#' See \code{\link[hcorp]{popular_1}} for more details.
#'
#' By default,
#' the dissonance estimation treats chords as transposition invariant,
#' and chord pitches as octave-invariant,
#' but differentiates between different inversions of the same chord.
#' Different behaviour can be achieved by passing a custom corpus analysis
#' to the \code{table} argument.
#' @param x Sonority to analyse.
#' This should be an object created by the \code{hrep} package,
#' representing a pitch chord (\code{\link[hrep]{pi_chord}}),
#' representing a pitch-class chord (\code{\link[hrep]{pc_chord}}),
#' representing a pitch-class chord type (\code{\link[hrep]{pc_chord_type}}),
#' or a pitch-class set (\code{\link[hrep]{pc_set}}).
#' This object will be coerced to the same type as the corpus dissonance table,
#' i.e. \code{type(table)}.
#' @param table Corpus dissonance table, as created by
#' \code{\link{corpus_dissonance_table}()}.
#' This table summarises chord prevalences within a corpus.
#' The default is \code{\link{popular_1_pc_chord_type}}.
#' @return Dissonance estimate, as a numeric scalar.
#' @references
#' \insertAllCited{}
#' @export
corpus_dissonance <- function(x, table = incon::popular_1_pc_chord_type) {
  typ <- type(table)
  x <- hrep::represent(x, typ)
  i <- hrep::encode(x)
  table$neg_log_prob[i]
}

#' Count chords
#'
#' This function counts chords within a corpus.
#' @param x Corpus to analyse.
#' @param type Representation to which chords should be coerced
#' before counting.
#' @return Integer vector providing the observed counts for each chord,
#' indexed by the type encoding defined in the \code{hrep} package.
#' @rdname count_chords
#' @export
count_chords <- function(x, type = "pc_chord_type") {
  UseMethod("count_chords")
}

#' @rdname count_chords
#' @export
count_chords.corpus <- function(x, type = "pc_chord_type") {
  hrep::represent(x, type) %>%
    do.call(c, .) %>%
    factor(levels = seq_len(hrep::alphabet_size(type))) %>%
    table %>%
    as.integer
}

