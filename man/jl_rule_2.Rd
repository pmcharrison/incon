% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/model-jl12.R
\name{jl_rule_2}
\alias{jl_rule_2}
\title{Tonal Dissonance, Rule 2}
\usage{
jl_rule_2(pc_set)
}
\arguments{
\item{pc_set}{(Numeric vector) Pitch-class set to analyse.
No input checking is performed.}
}
\value{
\code{TRUE} for dissonant, \code{FALSE} for consonant.
}
\description{
"Chords that are consistent with a major triad are more consonant
than chords that are not consistent with a major triad"
\insertCite{Johnson-Laird2012}{incon}.
Consistency with the major triad means that a major triad
must be contained within the chord.
Additionally, all notes must be contained within a major scale.
}
\references{
\insertAllCited{}
}
