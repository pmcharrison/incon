% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/model-dycon.R
\name{roughness_seth}
\alias{roughness_seth}
\alias{roughness_seth.default}
\alias{roughness_seth.sparse_fr_spectrum}
\title{Spectral roughness (Sethares)}
\usage{
roughness_seth(x, min_amplitude = TRUE, ...)

\method{roughness_seth}{default}(x, min_amplitude = TRUE, ...)

\method{roughness_seth}{sparse_fr_spectrum}(x, min_amplitude = TRUE, ...)
}
\arguments{
\item{x}{Object to analyse, which is coerced to the class
\code{\link[hrep]{sparse_fr_spectrum}}.
\itemize{
\item Numeric vectors will be treated as vectors of MIDI note numbers,
and expanded into their implied harmonics.
\item Two-element lists will be treated as finalised spectra,
with the first element being a numeric vector of frequencies,
and the second element being a numeric vector of amplitudes.
}}

\item{min_amplitude}{See \code{\link{dyad_roughness_seth}}.}

\item{...}{Further arguments to pass to \code{\link[hrep]{sparse_fr_spectrum}}.}
}
\value{
Estimated roughness, as a numeric scalar.
}
\description{
Gets the roughness of a sonority according to the model of Sethares (1993).
By default, the algorithm is modified according to
\insertCite{Sethares2005;textual}{incon} and
\insertCite{Weisser2013;textual}{incon}:
roughness is proportional to the minimum amplitude of each pair of partials,
not the product of their amplitudes.
This behaviour can be disabled by setting \code{min_amplitude = FALSE}.
}
\note{
\insertCite{Sethares2005;textual}{incon}
suggests using loudnesses instead of amplitudes.
However, he acknowledges that loudness is difficult to calculate
for arbitrary timbres.
Furthermore, if we replace amplitude with roughness,
we lose the original model's invariance to multiplicative
scaling of the original signal.
In this implementation, we therefore stay with amplitude,
consistent with \insertCite{Sethares1993;textual}{incon}.
}
\references{
\insertAllCited{}
}
