% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/quality_raters.R
\name{rate_std}
\alias{rate_std}
\title{Rate the acceptability of the standard deviation}
\usage{
rate_std(sd, .of = c("zscores", "raw_muac"))
}
\arguments{
\item{sd}{A vector of class \code{double} of standard deviation values from the
data set.}

\item{.of}{Specifies the data set to which the rating should be done. Can be
"wfhz", "mfaz", or "raw_muac".}
}
\value{
A vector of class \code{factor} of the same length as \code{sd} for the
acceptability rate.
}
\description{
Rate the acceptability of the standard deviation of WFHZ, MFAZ, and raw MUAC
data. Rating follows the SMART methodology criteria.
}
\keyword{internal}
