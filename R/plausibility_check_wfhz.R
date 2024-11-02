
#'
#' Check the plausibility and acceptability of a weight-for-height data
#'
#' @description
#' Verify the overall acceptability of the data through a set of
#' structured tests around sampling and measurement-related biases in the data.
#'
#' @param df A dataset object of class `data.frame` to check. It should have been
#' wrangled using this package's wranglers.
#'
#' @param sex A vector of class `numeric` of child's sex: 1 for boy and 2 for girl.
#'
#' @param age A vector of class `double` of child's age in months.
#'
#' @param weight A vector of class `double` of child's weight in kilograms.
#'
#' @param height A vector of class `double` of child's height in centimeters.
#'
#' @param flags A vector of class `numeric` of flagged observations.
#'
#' @export
#'
#' @examples
#' ## Check the plausibility of WFHZ data ----
#'
#' anthro.01 |>
#' mw_wrangle_age(
#' dos = dos,
#' dob = dob,
#' age = age,
#' .decimals = 2
#' ) |>
#' mw_wrangle_wfhz(
#' sex = sex,
#' weight = weight,
#' height = height,
#' .recode_sex = TRUE
#' ) |>
#' check_plausibility_wfhz(
#' sex = sex,
#' age = age,
#' weight = weight,
#' height = height,
#' flags = flag_wfhz,
#' )
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
        .for = "wfhz"),
      quality_class = rate_overall_quality(quality_score),
      .groups = "drop"
    )

  ## Return data frame ----
  df
}
