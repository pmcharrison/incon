% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/model-corpdiss.R
\name{count_chords}
\alias{count_chords}
\alias{count_chords.corpus}
\title{Count chords}
\usage{
count_chords(x, type = "pc_chord_type")

\method{count_chords}{corpus}(x, type = "pc_chord_type")
}
\arguments{
\item{x}{Corpus to analyse.}

\item{type}{Representation to which chords should be coerced
before counting.}
}
\value{
Integer vector providing the observed counts for each chord,
indexed by the type encoding defined in the \code{hrep} package.
}
\description{
This function counts chords within a corpus.
}
