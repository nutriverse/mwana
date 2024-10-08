

<!-- README.md is generated from README.Rmd. Please edit that file -->

# `mwana`: Utilities for analysing children’s nutritional status

<!-- badges: start -->

[![R-CMD-check](https://github.com/nutriverse/mwana/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/tomaszaba/ipccheckr/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/nutriverse/mwana/branch/main/graph/badge.svg)](https://app.codecov.io/gh/tomaszaba/ipccheckr?branch=main)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![Project Status: WIP – Initial development is in progress, but there
has not yet been a stable, usable release suitable for the
public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
<!-- badges: end -->

## Background

`mwana`, for “child” in *Elómwè*, a local language spoken in the
central-northern regions of Mozambique, with a similar meaning across
various other Bantu languages, including Swahili, spoken in many parts
of Africa, is a package designed for analysing anthropometric data, from
assessing quality of the data to computing prevalence, among `mwana`’s
aged 6 to 59 months.

`mwana` was born out of the author’s frequent wrestle when, in his
capacity as member of the Quality Assurance team for nutrition of the
IPC, was frequently presented with the task of handling large datasets
to conduct data quality and prevalence appraisal before every IPC
analysis to ensure the use of reliable evidence in the analyses. The
typical data appraisal workflow in the context of IPC is usually
cumbersome, as it requires significant time and effort, whilst ensuring
that the right analysis procedure is used by checking for different
conditionals. The data analysts often need to switch between software:
SPSS or Excel for data processing, then import data into ENA for SMART
software to run plausibility checks and prevalence analysis, then
extract outputs into a summary spreadsheet. This process is repeated one
by one for the number of units of analysis in the dataset. Oftentimes
this workflow needs to be implemented in relatively short period time,
leading to errors in the workflow due to fatigue.

In this way, more than just an R-based implementation of the ENA for
SMART software, `mwana`’s key added value lies in its ability to
simplify the above alluded cumbersome workflow into a wholesome
experience, all in one place. This is especially beneficial when
handling large datasets: a day-to-day practice at IPC.

> [!NOTE]
>
> `mwana` was made possible thanks to the state-of-the-art work in
> nutrition survey guidance led by the [SMART
> initiative](https://smartmethodology.org). Under to hood, `mwana`
> bundles the SMART guidance through the use of the National Information
> Platforms for Nutrition Anthropometric Data Toolkit (nipnTK)
> functionalities in `R` to build its handy function around plausibility
> checks. Click [here](https://github.com/nutriverse/nipnTK) to learn
> more about the `nipnTK` package.

## What does `mwana` do?

It automates plausibility checks and prevalence analyses and respective
summaries of the outputs.

### Plausibility checks.

- `mwana` performs plausibility checks on weight-for-height z-score
  (WFHZ)-based data by mimicking the SMART plausibility checkers in ENA
  for SMART software, their scoring and classification criterion.

- It performs, as well, plausibility checks on MUAC data. For this,
  `mwana` integrates recent advances in using MUAC-for-age z-score
  (MFAZ) for auditing the plausibility of MUAC data. In this way, when
  the variable age is available: `mwana` performs plausibility checks
  similar to those in WFHZ, however with few differences on the scoring.
  Otherwise, when the variables age is missing, a similar test suit used
  in the current version of ENA is performed. Read details here.

#### Useful workflow for plausibility check

<img src="man/figures/README-worflow-1.png" data-fig-align="center" />

### Prevalence analysis

`mwana` prevalence calculators were built to take decisions on the
appropriate analysis procedure to follow based on the quality of the
data, as per the SMART rules. It returns an output table with the
appropriate results based on the data quality test results.
Fundamentally, the calculators loop over the survey areas in the dataset
whilst performing quality appraisal and take decisions on the
appropriate prevalence analysis procedure to follow on the basis of the
result.

`mwana` computes prevalence for:

- Wasting on the basis of WFHZ and/edema (Read vignettes)
- Wasting on the basis of the absolute values of MUAC and/or edema:
  here, when variable age is available, mwana applies MFAZ flags,
  otherwise it applies the flagging criteria around the absolute values
  of MUAC, to exclude outliers before computing prevalence, but the
  actual prevalence is done on the absolute values. (Read link to the
  specific section in the vignettes)
- Wasting on the basis of MFAZ and/edema: outliers excluded using MFAZ
  flags. (Read link to the specific section in the vignettes)
- Combined prevalence of wasting: here a concept of combined flags is
  used to streamline the flags removed in WFHZ and those in MUAC. (Read
  link to the specific section in the vignettes).

`mwana` provides weighted prevalence analysis, if needed. And this is
controlled by the user. This is possible in all calculators, including
for MUAC, combined, which is not currently available in ENA for SMART.

In the context of IPC Acute Malnutrition (IPC AMN) analysis workflow,
`mwana` provides a handy function for checking if the minimum sample
size requirements in a given area were met on the basis of the
methodology used to collect the data: survey, screening or sentinel
sites. (Check out the vignette).

> [!TIP]
>
> If you are undertaking a research and you want to censor your data
> before including in your statistical models, etc, `mwana` is a great
> helper, as it identifies flags out of your anthro data.

> [!WARNING]
>
> Please note that `mwana` is still highly experimental and is
> undergoing a lot of development. Hence, any functionalities described
> below have a high likelihood of changing interface or approach as we
> aim for a stable working version.

## Installation

`mwana` is not yet on CRAN but you can install the development version
from [nutriverse R universe](https://nutriverse.r-universe.dev) as
follows:

``` r
remotes::install_github("tomaszaba/ipccheckr")
```

Then load to in memory with

``` r
library(ipccheckr)
```

# Citation

If you were enticed to use `mwana` package and found it useful, please
cite using the suggested citation provided by a call to `citation`
function as follows:

``` r
citation("ipccheckr")
#> To cite ipccheckr: in publications use:
#> 
#>   Tomás Zaba, Ernest Guevarra (2024). _ipccheckr: Toolkit for
#>   Performing IPC Acute Malnutrition-related Data Checks_. R package
#>   version 0.0.0.9000, <https://github.com/tomaszaba/ipccheckr>.
#> 
#> A BibTeX entry for LaTeX users is
#> 
#>   @Manual{,
#>     title = {ipccheckr: Toolkit for Performing IPC Acute Malnutrition-related Data Checks},
#>     author = {{Tomás Zaba} and {Ernest Guevarra}},
#>     year = {2024},
#>     note = {R package version 0.0.0.9000},
#>     url = {https://github.com/tomaszaba/ipccheckr},
#>   }
```

# Community guidelines

Feedback, bug reports and feature requests are welcome; file issues or
seek support [here](https://github.com/nutriverse/mwana/issues). If you
would like to contribute to the package, please see our [contributing
guidelines](https://nutriverse.io/mwana/CONTRIBUTING.html).

This project is releases with [Contributor Code of
Conduct](https://nutriverse.io/mwana/CODE_OF_CONDUCT.html). By
participating in this project you agree to abide by its terms.
