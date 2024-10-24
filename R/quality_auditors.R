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
#' @param area A vector of class `character` of the geographical location where
#' data was collected and for which the analysis should be aggregated.
#'
#' @returns A summarised `data.frame` of plausibility test results and their
#' respective acceptability ratings.
#'
#' @examples
#'
#' ## Check the plausibility of WFHZ data ----
#'
#' anthro.01 |>
#' mw_wrangle_age(
#' dos = dos,
#' dob = dob,
#' age = age
#' ) |>
#' process_wfhz_data(
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
#' area = area
#' )
#'
#' ## Check the plausibility of MFAZ data ----
#'
#' anthro.01 |>
#' mw_wrangle_age(
#' dos = dos,
#' dob = dob,
#' age = age
#' ) |>
#' process_muac_data(
#' sex = sex,
#' age = "age",
#' muac = muac,
#' .recode_sex = TRUE,
#' .recode_muac = TRUE,
#' unit = "cm"
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
#' process_muac_data(
#' sex = sex,
#' muac = muac,
#' age = NULL,
#' .recode_sex = TRUE,
#' .recode_muac = FALSE,
#' unit = "none"
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
    dplyr::group_by({{ area  }}) |>
    dplyr::summarise(
      n = n(),
      flagged = sum({{ flags }}, na.rm = TRUE) / n(),
      flagged_class = classify_percent_flagged(.data$flagged, type = "mfaz"),
      sex_ratio = sexRatioTest({{ sex }}, codes = c(1, 2))$p,
      sex_ratio_class = classify_age_sex_ratio(.data$sex_ratio),
      age_ratio = mw_stattest_ageratio({{ age }}, .expectedP = 0.66)$p,
      age_ratio_class = classify_age_sex_ratio(.data$age_ratio),
      dps = digitPreference({{ muac }}, digits = 1, values = 0:9)$dps,
      dps_class = digitPreference({{ muac }}, digits = 1, values = 0:9)$dpsClass,
      sd = sd(remove_flags(.data$mfaz, unit = "zscore"), na.rm = TRUE),
      sd_class = classify_sd(.data$sd, type = "zscore"),
      skew = skewKurt(remove_flags(.data$mfaz, unit = "zscore"))$s,
      skew_class = classify_skew_kurt(.data$skew),
      kurt = skewKurt(remove_flags(.data$mfaz, unit = "zscore"))$k,
      kurt_class = classify_skew_kurt(.data$kurt),
      .groups = "drop"
    )

  ## Add quality score to the data frame ----

  df[["quality_score"]] <- df |>
    group_by({{ area }}) |>
    compute_quality_score(type = "mfaz")

  ## Add quality class to the data frame ----

  df[["quality_class"]] <- df |>
    group_by({{ area }}) |>
    classify_overall_quality()

  ## Return data frame ----
  df
}


#'
#'
#' @rdname plausibility-check
#'
#' @export
#'
check_plausibility_wfhz <- function(df, sex, age, weight, height, flags, area) {


  ## Summarise statistics  ----
  df <- df |>
    group_by({{ area  }}) |>
    summarise(
      n = n(),
      flagged = sum({{ flags }}, na.rm = TRUE) / n(),
      flagged_class = classify_percent_flagged(.data$flagged, type = "whz"),
      sex_ratio = sexRatioTest({{ sex }}, codes = c(1, 2))$p,
      sex_ratio_class = classify_age_sex_ratio(.data$sex_ratio),
      age_ratio = ageRatioTest({{ age }}, ratio = 0.85)$p,
      age_ratio_class = classify_age_sex_ratio(.data$age_ratio),
      dps_wgt = digitPreference({{ weight }}, digits = 1)$dps,
      dps_wgt_class = digitPreference({{ weight }}, digits = 1)$dpsClass,
      dps_hgt = digitPreference({{ height }}, digits = 1)$dps,
      dps_hgt_class = digitPreference({{ height }}, digits = 1)$dpsClass,
      sd = sd(remove_flags(.data$wfhz, unit = "zscore"), na.rm = TRUE),
      sd_class = classify_sd(.data$sd, type = "zscore"),
      skew = skewKurt(remove_flags(.data$wfhz, unit = "zscore"))$s,
      skew_class = classify_skew_kurt(.data$skew),
      kurt = skewKurt(remove_flags(.data$wfhz, unit = "zscore"))$k,
      kurt_class = classify_skew_kurt(.data$kurt),
      .groups = "drop"
    )

  ## Add quality score to the data frame ----

  df[["quality_score"]] <- df |>
    group_by({{ area }}) |>
    compute_quality_score(type = "whz")

  ## Add quality class to the data frame ----

  df[["quality_class"]] <- df |>
    group_by({{ area }}) |>
    classify_overall_quality()

  ## Return data frame ----
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
      flagged_class = classify_percent_flagged(.data$flagged, type = "crude"),
      sex_ratio = sexRatioTest({{ sex }}, codes = c(1, 2))[["p"]],
      sex_ratio_class = classify_age_sex_ratio(.data$sex_ratio),
      dps = digitPreference({{ muac }}, digits = 0, values = 0:9)[["dps"]],
      dps_class = digitPreference({{ muac }}, digits = 0, values = 0:9)[["dpsClass"]],
      sd = sd(remove_flags({{ muac }}, unit = "crude"), na.rm = TRUE),
      sd_class = classify_sd(.data$sd, type = "crude"),
      .groups = "drop"
    )

  ## Return data frame ----
  df
}
