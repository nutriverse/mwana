

<!-- README.md is generated from README.Rmd. Please edit that file -->

# `mwana`: An efficient workflow for plausibility checks and prevalence analysis of wasting in R <img src="man/figures/logo.png" align="right" width="200px" />

<!-- badges: start -->

[![Project Status: WIP – Initial development is in progress, but there
has not yet been a stable, usable release suitable for the
public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![pages-build-deployment](https://github.com/nutriverse/mwana/actions/workflows/pages/pages-build-deployment/badge.svg)](https://github.com/nutriverse/mwana/actions/workflows/pages/pages-build-deployment)
[![R-CMD-check](https://github.com/nutriverse/mwana/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/nutriverse/mwana/actions/workflows/R-CMD-check.yaml)
[![test-coverage](https://github.com/nutriverse/mwana/actions/workflows/test-coverage.yaml/badge.svg)](https://github.com/nutriverse/mwana/actions/workflows/test-coverage.yaml)
[![codecov](https://codecov.io/gh/nutriverse/mwana/graph/badge.svg?token=kUUp1WOlSi)](https://codecov.io/gh/nutriverse/mwana)
[![CodeFactor](https://www.codefactor.io/repository/github/nutriverse/mwana/badge.png)](https://www.codefactor.io/repository/github/nutriverse/mwana)
[![DOI](https://zenodo.org/badge/867609177.svg)](https://zenodo.org/badge/latestdoi/867609177)
<!-- badges: end -->

Child anthropometric assessments are the cornerstones of child nutrition
and food security surveillance around the world. Ensuring the quality of
data from these assessments is paramount to obtaining accurate child
under nutrition prevalence estimates. Additionally, the timeliness of
reporting is, as well, critical to allowing timely situation analyses
and responses to tackle the needs of the affected population.

`mwana`, term for *child* in *Elómwè*, a local language spoken in the
central-northern regions of Mozambique, with a similar meaning across
other Bantu languages, such as Swahili, spoken in many parts of Africa,
is a package that streamlines data quality checks and wasting prevalence
estimation from anthropometric data of children aged 6 to 59 months old
through a comprehensive implementation of the SMART Methodology
guidelines in R.

## Motivation

`mwana` was borne out of the author’s own experience of having to work
with multiple child anthropometric data sets to conduct data quality
appraisal and prevalence estimation as part of the analysis Quality
Assurance Team of the Integrated Phase Classification (IPC) Global
Support Unit. The current standard child anthropometric data appraisal
workflow is extremely cumbersome, requiring significant time and effort
utilizing different software tools - SPSS, Excel, Emergency Nutrition
Assessment or ENA software - for each step of the process for a single
data set. This process is repeated for every data set needing to be
processed and often needing to be implemented in a relatively short
period of time. This manual and repetitive process, by its nature, is
extremely error-prone.

`mwana` simplifies this cumbersome workflow into a programmable process
particularly when handling multiple-area data set.

> [!NOTE]
>
> `mwana` was made possible thanks to the state-of-the-art work in
> nutrition survey guidance led by the [SMART
> initiative](https://smartmethodology.org). Under the hood, `mwana`
> bundles the SMART Methodology guidance, for both survey and non survey
> data, through the use of the National Information Platforms for
> Nutrition Anthropometric Data Toolkit (nipnTK) functionalities in `R`
> to build its handy function around plausibility checks and wasting
> prevalence estimation. Click
> [here](https://github.com/nutriverse/nipnTK) to learn more about the
> {`nipnTK`} package.

## What does `mwana` do?

It automates plausibility checks, prevalence analyses, and summary
outputs, providing particular advantages when handling data sets with
multiple areas.

### Plausibility checks.

- `mwana` performs plausibility checks on weight-for-height z-score
  (WFHZ) data by mimicking the SMART plausibility checkers in ENA for
  SMART software, their scoring and classification criterion. Read guide
  [here](https://nutriverse.io/mwana/articles/plausibility.html#plausibility-check-on-wfhz-data).

- It performs, as well, plausibility checks on MUAC data. For this,
  `mwana` integrates recent advances in using muac-for-age z-score
  (MFAZ) for checking the plausibility and the acceptability of MUAC
  data. In this way, when the variable age is available: `mwana`
  performs plausibility checks similar to those in WFHZ, with a few
  differences in the scoring criteria for the percent of flagged data.
  Otherwise, when the variables age is missing, a similar test suit used
  in the current version of ENA is performed. Read guide
  [here](https://nutriverse.io/mwana/articles/plausibility.html#plausibility-check-on-mfaz-data).

#### A tidy workflow for plausibility check using `mwana`

<p align="center" width="100%">
<div id="fig-workflow">

<img src="man/figures/workflow.png" style="width:80.0%" />


Figure 1: Plausibility check workflow using mwana

</div>
</p>

### Prevalence estimation

`mwana` prevalence estimators were built to take decisions on the
appropriate analysis procedure to follow based on the quality of the
data, as per the SMART rules. They return output tables with summarized
results based on the data quality test results. Fundamentally, the
functions loop over the survey areas in the data set whilst doing
quality checks and taking decisions on the appropriate prevalence
analysis path that best fits the data.

`mwana` estimates wasting prevalence on the basis of:

- WFHZ and/or edema. Read the guide
  [here](https://nutriverse.io/mwana/articles/prevalence.html#sec-prevalence-wfhz)
- Raw MUAC values and/or edema. When variable age is available,
  detection and removal of outliers is based on MFAZ, otherwise based on
  the raw MUAC values. This is simply to exclude outliers; the actual
  prevalence estimation is based on the raw MUAC values. Read the guide
  [here](https://nutriverse.io/mwana/articles/prevalence.html#sec-prevalence-muac).
- MFAZ and/or edema. Read the guide
  [here](https://nutriverse.io/mwana/articles/prevalence.html#estimation-of-the-prevalence-of-wasting-based-on-mfaz).
- Combined prevalence. A concept of combined flags is used to streamline
  the flags removed in WFHZ and those in MUAC. Read the guide
  [here](https://nutriverse.io/mwana/articles/prevalence.html#estimation-of-the-combined-prevalence-of-wasting).

In the context of IPC Acute Malnutrition (IPC AMN) analysis workflow,
`mwana` provides a handy function for checking whether the minimum
sample size requirements of a given area were met, on the basis of the
methodology used to collect the data, be it a survey, a screening or a
sentinel site data. Read the guide
[here](https://nutriverse.io/mwana/articles/ipc_amn_check.html).

> [!TIP]
>
> If you are undertaking a research and you want to wrangle your data
> before using it in your statistical models, `mwana` is a great helper.

> [!WARNING]
>
> Please note that `mwana` is still highly experimental and is
> undergoing a lot of development. Hence, any functionalities described
> above have a high likelihood of changing interface or approach as we
> aim for a stable working version.

## Installation

`mwana` is not yet on CRAN but can be installed from the [nutriverse R
Universe](https://nutriverse.r-universe.dev) as follows:

``` r
install.packages(
  "mwana",
  repos = c('https://nutriverse.r-universe.dev', 'https://cloud.r-project.org')
)
```

Then load to in memory with

``` r
library(mwana)
```

# Citation

If you were enticed to use `mwana` package and found it useful, please
cite using the suggested citation provided by a call to `citation`
function as follows:

``` r
citation("mwana")
#> To cite mwana: in publications use:
#> 
#>   Tomás Zaba, Ernest Guevarra (2024). _mwana: Utilities for Analysing
#>   Children's Nutritional Status_. R package version 0.0.0.9000,
#>   <https://github.com/nutriverse/mwana>.
#> 
#> A BibTeX entry for LaTeX users is
#> 
#>   @Manual{,
#>     title = {mwana: Utilities for Analysing Children's Nutritional Status},
#>     author = {{Tomás Zaba} and {Ernest Guevarra}},
#>     year = {2024},
#>     note = {R package version 0.0.0.9000},
#>     url = {https://github.com/nutriverse/mwana},
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
