% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/wranglers.R
\name{recode_muac}
\alias{recode_muac}
\title{Convert MUAC values to either centimeters or millimeters}
\usage{
recode_muac(muac, unit = c("cm", "mm"))
}
\arguments{
\item{muac}{A vector of class \code{double} or \code{integer} of the absolute MUAC values.}

\item{unit}{A choice of the unit to which the MUAC values should be converted.}
}
\value{
A numeric vector of the same length \code{muac}, with values converted
to the chosen unit.
}
\description{
Recode the MUAC values to either centimeters or millimeters as required.
}
\examples{

## Recode from millimeters to centimeters ----
muac <- anthro.01$muac
muac_cm <- recode_muac(muac, unit = "cm")

## Using the `muac_cm` object to recode it back to "mm" ----
muac_mm <- recode_muac(muac_cm, unit = "mm")

}
