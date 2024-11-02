#'
#'
#' Check the plausibility and acceptability of muac-for-age z-score (MFAZ) data
#'
#' @description
#' Check the overall plausibility and acceptability of MFAZ data through
#' structured test suite encompassing sampling and measurement-related biases in
#' the dataset. The test suite in this function follows the recommendation made
#' by Bilukha, O., & Kianian, B. (2023) on the plausibility of
#' constructing a comprehensive plausibility check similar to WFHZ to evaluate the
#' acceptability of MUAC data when the variable age exists in the dataset.
#'
#' The function works on a data frame returned from this package's wrangling
#' function for age and for MFAZ data.
#'
#' @param df A dataset object of class `data.frame` to check.
#'
#' @param sex A vector of class `numeric` of child's sex.
#'
#' @param age A vector of class `double` of child's age in months.
#'
#' @param muac A vector of class `numeric` of child's MUAC in centimeters.
#'
#' @param flags A vector of class `numeric` of flagged records.
#'
#' @returns
#' A summarised table of class `data.frame`, of length 17 and width 1, for
#' the plausibility test results and their respective acceptability ratings.
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
#'   age = age
#' )
#'
#' @export
#'
mw_plausibility_check_mfaz <- function(df, sex, muac, age, flags) {
  ## Summarise statistics  ----
  df <- df |>
    summarise(
      n = n(),
      flagged = sum({{ flags }}, na.rm = TRUE) / n(),
      flagged_class = rate_propof_flagged(.data$flagged, .in = "mfaz"),
      sex_ratio = sexRatioTest({{ sex }}, codes = c(1, 2))$p,
      sex_ratio_class = rate_agesex_ratio(.data$sex_ratio),
      age_ratio = mw_stattest_ageratio({{ age }}, .expectedP = 0.66)$p,
      age_ratio_class = rate_agesex_ratio(.data$age_ratio),
      dps = digitPreference({{ muac }}, digits = 1, values = 0:9)$dps,
      dps_class = digitPreference({{ muac }}, digits = 1, values = 0:9)$dpsClass,
      sd = sd(remove_flags(.data$mfaz, .from = "zscores"), na.rm = TRUE),
      sd_class = rate_std(.data$sd, .of = "zscores"),
      skew = skewKurt(remove_flags(.data$mfaz, .from = "zscores"))$s,
      skew_class = rate_skewkurt(.data$skew),
      kurt = skewKurt(remove_flags(.data$mfaz, .from = "zscores"))$k,
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
      .groups = "drop"
    )
  ## Return ----
  df
}


#'
#'
#' Clean and format the output table returned from the MFAZ plausibility check
#' for improved clarity and readability.
#'
#' @description
#' Clean and format the output table returned from the WFHZ plausibility check
#' for improved clarity and readability. It converts scientific notations to standard
#' notations, round values and rename columns to meaningful names.
#'
#' @param df A data frame containing the summary table returned by the package's
#' MFAZ plausibility checker function. Must be of class `data.frame`.
#'
#' @returns
#' A data frame of the same length and width as `df`, with column names and
#' values formatted for clarity.
#'
#' @examples
#' ## First wrangle age data ----
#' data <- mw_wrangle_age(
#' df = anthro.01,
#' dos = dos,
#' dob = dob,
#' age = age,
#' .decimals = 2
#' )
#'
#' ## Then wrangle MUAC data ----
#' data_mfaz <- mw_wrangle_muac(
#' df = data,
#' sex = sex,
#' age = age,
#' muac = muac,
#' .recode_sex = TRUE,
#' .recode_muac = TRUE,
#' .to = "cm"
#' )
#'
#' ## Then run plausibility check ----
#' pl <- mw_plausibility_check_mfaz(
#' df = data_mfaz,
#' flags = flag_mfaz,
#' sex = sex,
#' muac = muac,
#' age = age
#' )
#'
#' ## Now neat the output table ----
#' mw_neat_output_mfaz(df = pl)
#'
#' @export
#'
#'

mw_neat_output_mfaz <- function(df) {

  ## Format data frame ----
  df <- df |>
    mutate(
      flagged = .data$flagged |>
        label_percent(accuracy = 0.1, suffix = "%", decimal.mark = ".")(),
      sex_ratio = .data$sex_ratio |>
        label_pvalue()(),
      age_ratio = .data$age_ratio |>
        label_pvalue()(),
      sd = round(.data$sd, digits = 2),
      dps = round(.data$dps),
      skew = round(.data$skew, digits = 2),
      kurt = round(.data$kurt, digits = 2)
    ) |>
    ## Rename columns ----
  setNames(
    c("Total children", "Flagged data (%)",
      "Class. of flagged data", "Sex ratio (p)", "Class. of sex ratio",
      "Age ratio (p)", "Class. of age ratio", "DPS (#)",
      "Class. of DPS", "Standard Dev* (#)", "Class. of standard dev",
      "Skewness* (#)", "Class. of skewness", "Kurtosis* (#)",
      "Class. of kurtosis", "Overall score", "Overall quality"
    )
  )
  ## Return data frame ----
  df
}
