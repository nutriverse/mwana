#'
#' Check the plausibility and acceptability of weight-for-height z-score (WFHZ) data
#'
#' @description
#' Check the overall plausibility and acceptability of WFHZ data through
#' structured test suite encompassing sampling and measurement-related biases in
#' the dataset. This test suite, including the criteria and corresponding rating of
#' acceptability, follows the standards in the  SMART plausibility check. The only
#' exception is the exclusion of MUAC checks. MUAC is checked separately using more
#' comprehensive test suite as well.
#'
#' The function works on a data frame returned from this package's wrangling
#' function for age and for WFHZ data.
#'
#' @param df A dataset object of class `data.frame` to check.
#'
#' @param sex A vector of class `numeric` of child's sex.
#'
#' @param age A vector of class `double` of child's age in months.
#'
#' @param weight A vector of class `double` of child's weight in kilograms.
#'
#' @param height A vector of class `double` of child's height in centimeters.
#'
#' @param flags A vector of class `numeric` of flagged records.
#'
#' @returns
#' A summarised table of class `data.frame`, of length 19 and nrow 1, for
#' the plausibility test results and their respective acceptability ratings.
#'
#' @seealso [mw_plausibility_check_mfaz()] [mw_plausibility_check_muac()]
#' [mw_wrangle_age()]
#'
#' @references
#' SMART Initiative (2017). *Standardized Monitoring and Assessment for Relief
#' and Transition*. Manual 2.0. Available at: <https://smartmethodology.org>.
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
#' ## Then wrangle WFHZ data ----
#' data_wfhz <- mw_wrangle_wfhz(
#'   df = data,
#'   sex = sex,
#'   weight = weight,
#'   height = height,
#'   .recode_sex = TRUE
#' )
#'
#' ## Now run the plausibility check ----
#' mw_plausibility_check_wfhz(
#'   df = data_wfhz,
#'   sex = sex,
#'   age = age,
#'   weight = weight,
#'   height = height,
#'   flags = flag_wfhz,
#' )
#'
#' @export
#'
mw_plausibility_check_wfhz <- function(df,
                                       sex,
                                       age,
                                       weight,
                                       height,
                                       flags) {
  ## Summarise statistics  ----
  df <- df |>
    summarise(
      n = n(),
      flagged = sum({{ flags }}, na.rm = TRUE) / n(),
      flagged_class = rate_propof_flagged(.data$flagged, .in = "wfhz"),
      sex_ratio = sexRatioTest({{ sex }}, codes = c(1, 2))$p,
      sex_ratio_class = rate_agesex_ratio(.data$sex_ratio),
      age_ratio = ageRatioTest({{ age }}, ratio = 0.85)$p,
      age_ratio_class = rate_agesex_ratio(.data$age_ratio),
      dps_wgt = digitPreference({{ weight }}, digits = 1)$dps,
      dps_wgt_class = digitPreference({{ weight }}, digits = 1)$dpsClass,
      dps_hgt = digitPreference({{ height }}, digits = 1)$dps,
      dps_hgt_class = digitPreference({{ height }}, digits = 1)$dpsClass,
      sd = sd(remove_flags(.data$wfhz, .from = "zscores"), na.rm = TRUE),
      sd_class = rate_std(.data$sd, .of = "zscores"),
      skew = skewKurt(remove_flags(.data$wfhz, .from = "zscores"))$s,
      skew_class = rate_skewkurt(.data$skew),
      kurt = skewKurt(remove_flags(.data$wfhz, .from = "zscores"))$k,
      kurt_class = rate_skewkurt(.data$kurt),
      quality_score = score_overall_quality(
        cl_flags = .data$flagged_class,
        cl_sex = .data$sex_ratio_class,
        cl_age = .data$age_ratio_class,
        cl_dps_h = .data$dps_hgt_class,
        cl_dps_w = .data$dps_wgt_class,
        cl_std = .data$sd_class,
        cl_skw = .data$skew_class,
        cl_kurt = .data$kurt_class,
        .for = "wfhz"
      ),
      quality_class = rate_overall_quality(quality_score),
      .groups = "drop"
    )

  ## Return data frame ----
  df
}
