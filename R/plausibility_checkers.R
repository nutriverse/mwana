#'
#' Check the plausibility of the data
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
#' @param muac A vector of class `double` of child's MUAC in centimeters.
#'
#' @param weight A vector of class `double` of child's weight in kilograms.
#'
#' @param height A vector of class `double` of child's height in centimeters.
#'
#' @param flags A vector of class `numeric` of flagged observations.
#'
#' @returns A summarised `data.frame` of plausibility test results and their
#' respective acceptability ratings.
#'
#' @examples
#'
#' ## Check the plausibility of MFAZ data ----
#'
#' anthro.01 |>
#' mw_wrangle_age(
#' dos = dos,
#' dob = dob,
#' age = age,
#' .decimals = 2
#' ) |>
#' mw_wrangle_muac(
#' sex = sex,
#' age = "age",
#' muac = muac,
#' .recode_sex = TRUE,
#' .recode_muac = TRUE,
#' .to = "cm"
#' ) |>
#' check_plausibility_mfaz(
#' flags = flag_mfaz,
#' sex = sex,
#' muac = muac,
#' age = age,
#' area = area
#' )
#'
#' ## Check the plausibility of the absolute MUAC values ----
#'
#' anthro.01 |>
#' mw_wrangle_muac(
#' sex = sex,
#' muac = muac,
#' age = NULL,
#' .recode_sex = TRUE,
#' .recode_muac = FALSE,
#' .to = "none"
#' ) |>
#' check_plausibility_muac(
#' flags = flag_muac,
#' sex = sex,
#' muac = muac
#' )
#'
#' @rdname plausibility-check
#'
#' @export
#'
check_plausibility_mfaz <- function(df, sex, muac, age, flags, area) {

  ## Summarise statistics  ----
  df <- df |>
    group_by({{ area  }}) |>
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
        .for = "mfaz"),
      quality_class = rate_overall_quality(quality_score),
      .groups = "drop"
    )
  ## Return ----
  df
}



#'
#' @rdname plausibility-check
#'
#' @export
#'
check_plausibility_muac <- function(df, flags, sex, muac) {

  ## Summarise statistics  ----
  df <- df |>
    summarise(
      n = n(),
      flagged = sum({{ flags }}, na.rm = TRUE) / n(),
      flagged_class = rate_propof_flagged(.data$flagged, .in = "raw_muac"),
      sex_ratio = sexRatioTest({{ sex }}, codes = c(1, 2))[["p"]],
      sex_ratio_class = rate_agesex_ratio(.data$sex_ratio),
      dps = digitPreference({{ muac }}, digits = 0, values = 0:9)[["dps"]],
      dps_class = digitPreference({{ muac }}, digits = 0, values = 0:9)[["dpsClass"]],
      sd = sd(remove_flags({{ muac }}, .from = "raw_muac"), na.rm = TRUE),
      sd_class = rate_std(.data$sd, .of = "raw_muac"),
      .groups = "drop"
    )

  ## Return data frame ----
  df
}
