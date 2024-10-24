% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/prevalence_wfhz.R
\name{apply_probit_approach}
\alias{apply_probit_approach}
\alias{compute_probit_prevalence}
\title{Compute the prevalence estimates of wasting on the basis of the PROBIT method.}
\usage{
apply_probit_approach(x, .status = c("gam", "sam"))

compute_probit_prevalence(df, .summary_by = NULL, .for = c("wfhz", "mfaz"))
}
\arguments{
\item{x}{A vector of class \code{double} of WFHZ or MFAZ values.}

\item{.status}{A choice of the form of wasting for which the prevalence should
be estimated.}

\item{df}{An already wrangled dataset object of class \code{data.frame} to use.}

\item{.summary_by}{A vector of class \code{character} of the geographical areas
where the data was collected and for which the analysis should be performed.}

\item{.for}{A choice between "wfhz" and "mfaz" for the anthropometric index.}
}
\value{
A summarised table of class \code{data.frame} of the prevalence estimates.
No confidence intervals are yielded.
}
\description{
This approach is applied when the standard deviation of WFHZ is problematic.
The PROBIT method estimates the prevalence of wasting indirectly by calculating
the area under the tail of the curve, from negative infinitive to
the given threshold, using the cumulative normal distribution function with
the mean and standard deviation as inputs.
}
