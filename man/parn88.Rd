% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/model-parn88.R
\name{parn88}
\alias{parn88}
\alias{parn88.default}
\alias{parn88.pc_set}
\title{Parncutt (1988)}
\usage{
parn88(x, root_support = "v2", exponent = 0.5)

\method{parn88}{default}(x, root_support = "v2", exponent = 0.5)

\method{parn88}{pc_set}(x, root_support = "v2", exponent = 0.5)
}
\arguments{
\item{x}{Sonority to analyse.
This will be coerced to an object of class \code{\link[hrep]{pc_set}}.}

\item{root_support}{(Character scalar or data frame)
Identifies the root support weights to use.
\itemize{
\item \code{"v2"} (default) uses the updated
weights from \insertCite{Parncutt2006;textual}{incon}.
\item \code{"v1"} uses the original weights from \insertCite{Parncutt2006;textual}{incon}.
}

See \code{\link{root_support_weights}} for the values of these weights.
Alternatively, root-support weights can be provided as a data frame,
with one column (interval) identifying the ascending interval in semitones,
and another column (weight) identifying the corresponding root support weight.}

\item{exponent}{(Numeric scalar) Exponent to be used when computing
root ambiguities. Defaults to 0.5, after \insertCite{Parncutt1988;textual}{incon}.}
}
\value{
A list with three values:
\itemize{
\item \code{root}, the estimated chord root (integer scalar);
\item \code{root_ambiguity}, the root ambiguity (numeric scalar),
\item \code{pc_weight}, a 12-dimensional vector of weights by pitch class.
}
}
\description{
Analyses a pitch-class set using the root-finding model of
\insertCite{Parncutt1988;textual}{incon}.
}
\references{
\insertAllCited{}
}
