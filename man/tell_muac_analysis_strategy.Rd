% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/prevalence_muac.R
\name{tell_muac_analysis_strategy}
\alias{tell_muac_analysis_strategy}
\title{A helper function to determine the MUAC prevalence analysis approach to follow}
\usage{
tell_muac_analysis_strategy(age_ratio_class, sd_class)
}
\arguments{
\item{age_ratio_class}{A vector of class \code{character} of the acceptability
classification of the age ratio test result.}

\item{sd_class}{A vector of class \code{character} of the acceptability
classification of the standard deviation analysis result.}
}
\value{
A vector of class \code{character} of the same length as the input vectors,
containing values indicating the analysis approach for each analysis area: "weighted",
"unweighted" and "missing".
}
\description{
It determines the analysis approach to follow for a given analysis area on
the basis of the rate of acceptability of the age ratio test and the standard
deviation analysis result.
}
\details{
When "weighted", the CDC weighting approach is applied to correct for
age bias; when "unweighted" a normal complex sample analysis is applied; when
"missing" \code{NA} gets thrown.
}
