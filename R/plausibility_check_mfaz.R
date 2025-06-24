#'
#' Check the plausibility and acceptability of MUAC-for-age z-score (MFAZ) data
#'
#' @description
#' Check the overall plausibility and acceptability of MFAZ data through a
#' structured test suite encompassing checks for sampling and
#' measurement-related biases in the dataset. This test suite follows the
#' recommendation made by Bilukha & Kianian (2023) on the plausibility of
#' constructing a comprehensive plausibility check for MUAC data similar to
#' weight-for-height z-score to evaluate its acceptability when age values are
#' available in the dataset.
#'
#' The function works on a `data.frame` returned from wrangling functions for
#' age and for MUAC-for-age z-score data available from this package.
#'
#' @param df A `data.frame` object to check.
#'
#' @param sex A `numeric` vector for child's sex with 1 = males and 2 = females.
#'
#' @param age A vector of class `double` of child's age in months.
#'
#' @param muac A `numeric` vector of child's MUAC in centimeters.
#'
#' @param flags A `numeric` vector of flagged records.
#'
#' @param .by A `character` or `numeric` vector of the geographical areas for
#' where the data was collected and for which the analysis should be summarised
#' for.
#'
#' @returns A single row summary `tibble` with 17 columns (if ungrouped analysis,
#' otherwise 18), containing the plausibility check results and their respective
#' acceptability ratings.
#'
#' @details
#' Whilst the function uses the same checks and criteria as those for
#' weight-for-height z-scores in the SMART plausibility check, the percent of
#' flagged records is evaluated using different cut-off points, with a maximum
#' acceptability of 2.0% as shown below:
#'
#' |**Excellent** | **Good** | **Acceptable** | **Problematic** |
#' | :---: | :---: | :---: | :---: |
#' | 0.0 - 1.0 | >1.0 - 1.5| >1.5 - 2.0 | >2.0  |
#'
#' @references
#' Bilukha, O., & Kianian, B. (2023). Considerations for assessment of measurement
#' quality of mid‐upper arm circumference data in anthropometric surveys and
#' mass nutritional screenings conducted in humanitarian and refugee settings.
#' *Maternal & Child Nutrition*, 19, e13478. <https://doi.org/10.1111/mcn.13478>
#'
#' SMART Initiative (2017). *Standardized Monitoring and Assessment for Relief
#' and Transition*. Manual 2.0. Available at: <https://smartmethodology.org>.
#'
#' @seealso [mw_wrangle_age()] [mw_wrangle_muac()] [mw_stattest_ageratio()]
#' [flag_outliers()]
#'
#' @examples
#' ## First wrangle age data ----
#' data <- mw_wrangle_age(
#'   df = anthro.01,
#'   dos = dos,
#'   dob = dob,
#'   age = age,
#'   .decimals = 2
#' )
#'
#' ## Then wrangle MUAC data ----
#' data_muac <- mw_wrangle_muac(
#'   df = data,
#'   sex = sex,
#'   age = age,
#'   muac = muac,
#'   .recode_sex = TRUE,
#'   .recode_muac = TRUE,
#'   .to = "cm"
#' )
#'
#' ## And finally run plausibility check ----
#' mw_plausibility_check_mfaz(
#'   df = data_muac,
#'   flags = flag_mfaz,
#'   sex = sex,
#'   muac = muac,
#'   age = age,
#'   .by = area
#' )
#'
#' @export
#'
mw_plausibility_check_mfaz <- function(df, sex, muac, age, flags, .by = NULL) {
  ## Difuse argument `.by` ----
  .by <- enquo(.by)

  if (rlang::quo_is_null(.by)) {
    ## Summarise statistics  ----
    df <- dplyr::summarise(
      .data = df,
      n = dplyr::n(),
      flagged = sum({{ flags }}, na.rm = TRUE) / dplyr::n(),
      flagged_class = rate_propof_flagged(.data$flagged, .in = "mfaz"),
      sex_ratio = nipnTK::sexRatioTest({{ sex }}, codes = c(1, 2))$p,
      sex_ratio_class = rate_agesex_ratio(.data$sex_ratio),
      age_ratio = mw_stattest_ageratio({{ age }}, .expectedP = 0.66)$p,
      age_ratio_class = rate_agesex_ratio(.data$age_ratio),
      dps = nipnTK::digitPreference({{ muac }}, digits = 1, values = 0:9)$dps,
      dps_class = nipnTK::digitPreference(
        {{ muac }},
        digits = 1, values = 0:9
      )$dpsClass,
      sd = stats::sd(remove_flags(.data$mfaz, .from = "zscores"), na.rm = TRUE),
      sd_class = rate_std(.data$sd, .of = "zscores"),
      skew = nipnTK::skewKurt(remove_flags(.data$mfaz, .from = "zscores"))$s,
      skew_class = rate_skewkurt(.data$skew),
      kurt = nipnTK::skewKurt(remove_flags(.data$mfaz, .from = "zscores"))$k,
      kurt_class = rate_skewkurt(.data$kurt),
      quality_score = score_overall_quality(
        cl_flags = .data$flagged_class,
        cl_sex = .data$sex_ratio_class,
        cl_age = .data$age_ratio_class,
        cl_dps_m = .data$dps_class,
        cl_std = .data$sd_class,
        cl_skw = .data$skew_class,
        cl_kurt = .data$kurt_class,
        .for = "mfaz"
      ),
      quality_class = rate_overall_quality(.data$quality_score)
    )
  }
  if (!rlang::quo_is_null(.by)) {
    ## Summarise statistics  ----
    df <- dplyr::summarise(
      .data = df,
      n = dplyr::n(),
      flagged = sum({{ flags }}, na.rm = TRUE) / dplyr::n(),
      flagged_class = rate_propof_flagged(.data$flagged, .in = "mfaz"),
      sex_ratio = nipnTK::sexRatioTest({{ sex }}, codes = c(1, 2))$p,
      sex_ratio_class = rate_agesex_ratio(.data$sex_ratio),
      age_ratio = mw_stattest_ageratio({{ age }}, .expectedP = 0.66)$p,
      age_ratio_class = rate_agesex_ratio(.data$age_ratio),
      dps = nipnTK::digitPreference({{ muac }}, digits = 1, values = 0:9)$dps,
      dps_class = nipnTK::digitPreference(
        {{ muac }},
        digits = 1, values = 0:9
      )$dpsClass,
      sd = stats::sd(remove_flags(.data$mfaz, .from = "zscores"), na.rm = TRUE),
      sd_class = rate_std(.data$sd, .of = "zscores"),
      skew = nipnTK::skewKurt(remove_flags(.data$mfaz, .from = "zscores"))$s,
      skew_class = rate_skewkurt(.data$skew),
      kurt = nipnTK::skewKurt(remove_flags(.data$mfaz, .from = "zscores"))$k,
      kurt_class = rate_skewkurt(.data$kurt),
      quality_score = score_overall_quality(
        cl_flags = .data$flagged_class,
        cl_sex = .data$sex_ratio_class,
        cl_age = .data$age_ratio_class,
        cl_dps_m = .data$dps_class,
        cl_std = .data$sd_class,
        cl_skw = .data$skew_class,
        cl_kurt = .data$kurt_class,
        .for = "mfaz"
      ),
      quality_class = rate_overall_quality(.data$quality_score),
      .by = !!.by
    )
  }

  ## Return data.frame ----
  df
}



#'
#' Clean and format the output tibble returned from the MUAC-for-age z-score
#' plausibility check
#'
#' @description
#' Converts scientific notations to standard notations, rounds off values, and
#' renames columns to meaningful names.
#'
#' @param df An `data.frame` object returned by [mw_plausibility_check_mfaz()]
#' containing the summarized results to be formatted.
#'
#' @param .by A `character` or `numeric` vector of the geographical areas for
#' where the data was collected and for which the analysis should be summarised
#' for.
#'
#' @returns
#' A `data.frame` object of the same length and width as `df`, with column
#' names and values formatted as appropriate.
#'
#' @examples
#' ## First wrangle age data ----
#' data <- mw_wrangle_age(
#'   df = anthro.01,
#'   dos = dos,
#'   dob = dob,
#'   age = age,
#'   .decimals = 2
#' )
#'
#' ## Then wrangle MUAC data ----
#' data_mfaz <- mw_wrangle_muac(
#'   df = data,
#'   sex = sex,
#'   age = age,
#'   muac = muac,
#'   .recode_sex = TRUE,
#'   .recode_muac = TRUE,
#'   .to = "cm"
#' )
#'
#' ## Then run plausibility check ----
#' pl <- mw_plausibility_check_mfaz(
#'   df = data_mfaz,
#'   flags = flag_mfaz,
#'   sex = sex,
#'   muac = muac,
#'   age = age,
#'   .by = area
#' )
#'
#' ## Now neat the output table ----
#' mw_neat_output_mfaz(df = pl, .by = area)
#'
#' @export
#'
mw_neat_output_mfaz <- function(df, .by = NULL) {
  ## Difuse argument `.by` ----
  .by <- enquo(.by)

  df <- dplyr::mutate(
    .data = df,
    flagged = scales::label_percent(
      accuracy = 0.1, suffix = "%", decimal.mark = "."
    )(.data$flagged),
    sex_ratio = scales::label_pvalue()(.data$sex_ratio),
    age_ratio = scales::label_pvalue()(.data$age_ratio),
    sd = round(.data$sd, digits = 2),
    dps = round(.data$dps),
    skew = round(.data$skew, digits = 2),
    kurt = round(.data$kurt, digits = 2)
  ) |>
    ## Rename columns ----
    stats::setNames(
      c(
        if (!rlang::quo_is_null(.by)) "Group" else NULL,
        "Total children", "Flagged data (%)",
        "Class. of flagged data", "Sex ratio (p)", "Class. of sex ratio",
        "Age ratio (p)", "Class. of age ratio", "DPS (#)",
        "Class. of DPS", "Standard Dev* (#)", "Class. of standard dev",
        "Skewness* (#)", "Class. of skewness", "Kurtosis* (#)",
        "Class. of kurtosis", "Overall score", "Overall quality"
      )
    )
  ## Return data.frame ----
  df
}
