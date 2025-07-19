# mwana 0.2.2

## New features

* Added new functions:
    - `mw_stattest_ageratio2()`: Tests for statistical differences in proportions as in `[mw_stattest_ageratio()]`, but for cases where age is provided in the categories “6–23” and “24–59” months.

    - `mw_estimate_prevalence_screening2()`: Estimates the prevalence of acute malnutrition for non-survey data when age is given in the categories “6–23” and “24–59” months. This ensures that the age-weighting approach is applied when applicable. Outliers are excluded using raw MUAC rather than MUAC-for-age z-scores.

## Bug fixes 

* Resolved an issue with `mw_check_ipcamn_ssreq()`, which was previously not returning the correct statistics for grouped analysis.

## General updates 

* Refactored plausibility check and prevalence functions to allow multiple grouping variables (rather than just one, as was the case in earlier versions). The `.by` argument is no longer used; instead, users must specify the variable or set of variables using `...` .

* Reduced repeated code in several function definitions.

<br/>

# mwana 0.2.1

## General updates

* Updated documentation in README, data documentation function documentation, and vignettes to improve grammar, coherence, and consistency;

* Enforced use of `::` to state external package dependencies;

* Ensured that a code sequence started with a function statement rather than a data.frame piped into a function;

* simplified specific code syntax

<br/>

# mwana v0.2.0

## New features

* Added new function `mw_estimate_prevalence_screening()` to estimate prevalence 
of wasting by MUAC from non survey data: screenings, sentinel sites, etc.

## Bug fixes

* Resolved issues with `mw_neat_output_mfaz()`, `mw_neat_output_wfhz()` and 
`mw_neat_output_muac()` not returning neat and tidy output for grouped `data.frame` from their respective plausibility checkers.

* Resolved issue with `edema` argument in prevalence functions that was not working as expected when set to `NULL`.

## General updates

* Updated general package documentation, including references in vignettes. 
* Built package using R version 4.4.2

<br/>

# mwana v0.1.0

* Initial pre-release version for alpha-testing.
