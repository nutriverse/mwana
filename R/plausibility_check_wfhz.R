#'
#' Check the plausibility and acceptability of weight-for-height z-score (WFHZ)
#' data
#'
#' @description
#' Check the overall plausibility and acceptability of WFHZ data through a
#' structured test suite encompassing checks for sampling and
#' measurement-related biases in the dataset. The test suite, including the
#' criteria and corresponding rating of acceptability, follows the standards in
#' the SMART plausibility check.
#'
#' The function works on a data frame returned by this package's wrangling
#' functions for age and for WFHZ data.
#'
#' @param df A `tibble` object to check.
#'
#' @param sex A `numeric` vector for child's sex with 1 = males and 2 = females.
#'
#' @param age A vector of class `double` of child's age in months.
#'
#' @param weight A vector of class `double` of child's weight in kilograms.
#'
#' @param height A vector of class `double` of child's height in centimeters.
#'
#' @param flags A `numeric` vector of flagged records.
#'
#' @param .by A `character` or `numeric` vector of the geographical areas for
#' where the data was collected and for which the analysis should be summarised
#' for.
#'
#' @returns
#' A single row summary `tibble` with 19 columns (if ungrouped analysis, 
#' otherwise 20) for the plausibility check results and their respective 
#' acceptability rates.
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
#'   .by = area
#' )
#'
#' @export
#'

mw_plausibility_check_wfhz <- function(df,
                                       sex,
                                       age,
                                       weight,
                                       height,
                                       flags,
                                       .by = NULL) {
  ## Difuse argument `.by` for later evaluation ----
  .by <- enquo(.by)

  if (rlang::quo_is_null(.by)) {
    ## Summarise statistics  ----
    df <- dplyr::summarise(
      .data = df,
      n = dplyr::n(),
      flagged = sum({{ flags }}, na.rm = TRUE) / n(),
      flagged_class = rate_propof_flagged(.data$flagged, .in = "wfhz"),
      sex_ratio = nipnTK::sexRatioTest({{ sex }}, codes = c(1, 2))$p,
      sex_ratio_class = rate_agesex_ratio(.data$sex_ratio),
      age_ratio = nipnTK::ageRatioTest({{ age }}, ratio = 0.85)$p,
      age_ratio_class = rate_agesex_ratio(.data$age_ratio),
      dps_wgt = nipnTK::digitPreference({{ weight }}, digits = 1)$dps,
      dps_wgt_class = nipnTK::digitPreference({{ weight }}, digits = 1)$dpsClass,
      dps_hgt = nipnTK::digitPreference({{ height }}, digits = 1)$dps,
      dps_hgt_class = nipnTK::digitPreference({{ height }}, digits = 1)$dpsClass,
      sd = stats::sd(remove_flags(.data$wfhz, .from = "zscores"), na.rm = TRUE),
      sd_class = rate_std(.data$sd, .of = "zscores"),
      skew = nipnTK::skewKurt(remove_flags(.data$wfhz, .from = "zscores"))$s,
      skew_class = rate_skewkurt(.data$skew),
      kurt = nipnTK::skewKurt(remove_flags(.data$wfhz, .from = "zscores"))$k,
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
      quality_class = rate_overall_quality(.data$quality_score)
    )
  }

  if (!quo_is_null(.by)) {
    ## Summarise statistics  ----
    df <- dplyr::summarise(
      .data = df,
      n = dplyr::n(),
      flagged = sum({{ flags }}, na.rm = TRUE) / n(),
      flagged_class = rate_propof_flagged(.data$flagged, .in = "wfhz"),
      sex_ratio = nipnTK::sexRatioTest({{ sex }}, codes = c(1, 2))$p,
      sex_ratio_class = rate_agesex_ratio(.data$sex_ratio),
      age_ratio = nipnTK::ageRatioTest({{ age }}, ratio = 0.85)$p,
      age_ratio_class = rate_agesex_ratio(.data$age_ratio),
      dps_wgt = nipnTK::digitPreference({{ weight }}, digits = 1)$dps,
      dps_wgt_class = nipnTK::digitPreference({{ weight }}, digits = 1)$dpsClass,
      dps_hgt = nipnTK::digitPreference({{ height }}, digits = 1)$dps,
      dps_hgt_class = nipnTK::digitPreference({{ height }}, digits = 1)$dpsClass,
      sd = stats::sd(remove_flags(.data$wfhz, .from = "zscores"), na.rm = TRUE),
      sd_class = rate_std(.data$sd, .of = "zscores"),
      skew = nipnTK::skewKurt(remove_flags(.data$wfhz, .from = "zscores"))$s,
      skew_class = rate_skewkurt(.data$skew),
      kurt = nipnTK::skewKurt(remove_flags(.data$wfhz, .from = "zscores"))$k,
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
      quality_class = rate_overall_quality(.data$quality_score),
      .by = !!.by
    )
  }

  ## Return data.frame ----
  df
}

#'
#' Clean and format the output tibble returned from the WFHZ plausibility check
#'
#' @description
#' Converts scientific notations to standard notations, rounds off values, and
#' renames columns to meaningful names.
#'
#' @param df An `tibble` object returned by the [mw_plausibility_check_wfhz()]
#' containing the summarized results to be formatted.
#' 
#' @param .by A `character` or `numeric` vector of the geographical areas for
#' where the data was collected and for which the analysis should be summarised
#' for.
#'
#' @returns
#' A `tibble` object of the same length and width as `df`, with column names and
#' values formatted for clarity and readability.
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
#' pl <- mw_plausibility_check_wfhz(
#'   df = data_wfhz,
#'   sex = sex,
#'   age = age,
#'   weight = weight,
#'   height = height,
#'   flags = flag_wfhz,
#'   .by = area
#' )
#'
#' ## Now neat the output table ----
#' mw_neat_output_wfhz(df = pl, .by = area)
#'
#' @export

mw_neat_output_wfhz <- function(df, .by = NULL) {
  ## Difuse argument `.by` for later evaluation ----
  .by <- enquo(.by)

  if (quo_is_null(.by)) {
    ## Format data frame ----
    df <- dplyr::mutate(
      .data = df,
      flagged = scales::label_percent(
        accuracy = 0.1, suffix = "%", decimal.mark = "."
      )(.data$flagged),
      sex_ratio = scales::label_pvalue()(.data$sex_ratio),
      age_ratio = scales::label_pvalue()(.data$age_ratio),
      sd = round(.data$sd, digits = 2),
      dps_wgt = round(.data$dps_wgt),
      dps_hgt = round(.data$dps_hgt),
      skew = round(.data$skew, digits = 2),
      kurt = round(.data$kurt, digits = 2)
    ) |>
      ## Rename columns ----
      stats::setNames(
        c(
          "Total children", "Flagged data (%)", "Class. of flagged data",
          "Sex ratio (p)", "Class. of sex ratio", "Age ratio (p)",
          "Class. of age ratio", "DPS weight (#)", "Class. DPS weight",
          "DPS height (#)", "Class. DPS height", "Standard Dev* (#)",
          "Class. of standard dev", "Skewness* (#)", "Class. of skewness",
          "Kurtosis* (#)", "Class. of kurtosis", "Overall score",
          "Overall quality"
        )
      )
  } else {
    ## Format data frame adding the variable `Group` ----
    df <- dplyr::mutate(
      .data = df,
      flagged = scales::label_percent(
        accuracy = 0.1, suffix = "%", decimal.mark = "."
      )(.data$flagged),
      sex_ratio = scales::label_pvalue()(.data$sex_ratio),
      age_ratio = scales::label_pvalue()(.data$age_ratio),
      sd = round(.data$sd, digits = 2),
      dps_wgt = round(.data$dps_wgt),
      dps_hgt = round(.data$dps_hgt),
      skew = round(.data$skew, digits = 2),
      kurt = round(.data$kurt, digits = 2)
    ) |>
      ## Rename columns ----
      stats::setNames(
        c(
          "Group",
          "Total children", "Flagged data (%)", "Class. of flagged data",
          "Sex ratio (p)", "Class. of sex ratio", "Age ratio (p)",
          "Class. of age ratio", "DPS weight (#)", "Class. DPS weight",
          "DPS height (#)", "Class. DPS height", "Standard Dev* (#)",
          "Class. of standard dev", "Skewness* (#)", "Class. of skewness",
          "Kurtosis* (#)", "Class. of kurtosis", "Overall score",
          "Overall quality"
        )
      )
  }

  ## Return data.frame ----
  df
}
