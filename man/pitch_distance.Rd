% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/model-parn94.R
\name{pitch_distance}
\alias{pitch_distance}
\title{Get pitch distance}
\usage{
pitch_distance(x, y, ...)
}
\arguments{
\item{x}{The first sonority to compare, passed to \code{\link{pitch_salience}()}.
Typically will be a numeric vector of MIDI pitches.}

\item{y}{The second sonority to compare, passed to \code{\link{pitch_salience}()}.
Typically will be a numeric vector of MIDI pitches.}

\item{...}{Further arguments to pass to \code{\link{pitch_salience}()}.}
}
\value{
Pitch distance, as a numeric scalar.
}
\description{
Gets the pitch distance between two sonorities, after
\insertCite{Parncutt1994;textual}{incon}.
}
\references{
\insertAllCited{}
}
