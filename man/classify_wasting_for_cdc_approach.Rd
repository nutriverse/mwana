% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/case_definitions.R
\name{classify_wasting_for_cdc_approach}
\alias{classify_wasting_for_cdc_approach}
\title{Classify wasting into severe or moderate wasting to be used in the
SMART MUAC tool weighting approach}
\usage{
classify_wasting_for_cdc_approach(muac, .edema = NULL)
}
\arguments{
\item{muac}{A vector of class \code{integer} of MUAC values in millimeters.}

\item{.edema}{A vector of class \code{character} of edema. Code should be
"y" for presence and "n" for absence of bilateral edema. Default is \code{NULL}.}
}
\value{
A vector of class \code{character} of the same length as \code{muac} and \code{.edema}
indicating if a child is severe or moderately wasted or not wasted.
}
\description{
Classify wasting into severe or moderate wasting to be used in the
SMART MUAC tool weighting approach
}
