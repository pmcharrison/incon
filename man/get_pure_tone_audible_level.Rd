% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/model-parn94.R
\name{get_pure_tone_audible_level}
\alias{get_pure_tone_audible_level}
\title{Get audible level}
\usage{
get_pure_tone_audible_level(auditory_level, overall_masking_level)
}
\arguments{
\item{auditory_level}{Numeric vector of auditory levels for
a set of pure tones (dB).}

\item{overall_masking_level}{Numeric vector of overall masking levels
for a set of pure tones (dB).}
}
\value{
Numeric vector of audible levels (dB).
}
\description{
Returns the audible level for set of pure tones subject
to a given masking pattern.
Corresponds to Equation 6 of \insertCite{Parncutt1994;textual}{incon}.
}
\references{
\insertAllCited{}
}
