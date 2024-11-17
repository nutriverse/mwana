#'
#' Check the plausibility and acceptability of weight-for-height z-score (WFHZ) data
#'
#' @description
#' Check the overall plausibility and acceptability of WFHZ data through a
#' structured test suite encompassing sampling and measurement-related biases checks
#' in the data set. The test suite, including the criteria and corresponding rating of
#' acceptability, follows the standards in the  SMART plausibility check. The only
#' exception is the exclusion of MUAC checks. MUAC is checked separately using more
#' comprehensive test suite as well.
#'
#' The function works on a data frame returned from this package's wrangling
#' function for age and for WFHZ data.
#'
#' @param df A data set object of class `data.frame` to check.
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
#' A summarized table of class `data.frame`, of length 19 and width 1, for
#' the plausibility test results and their respective acceptability rates.
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
#'   flags = flag_wfhz
#' )
#'
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
      quality_class = rate_overall_quality(.data$quality_score),
      .groups = "drop"
    )

  ## Return data frame ----
  df
}

#'
#'
#' Clean and format the output table returned from the WFHZ plausibility check
#' for improved clarity and readability
#'
#' @description
#' Clean and format the output table returned from the WFHZ plausibility check
#' for improved clarity and readability. It converts scientific notations to standard
#' notations, round values and rename columns to meaningful names.
#'
#' @param df An object of class `data.frame` returned by this package's
#' plausibility checker for WFHZ data, containing the summarized results to be
#' formatted.
#'
#' @returns
#' A `data.frame` object of the same length and width as `df`, with column names and
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
#'   flags = flag_wfhz
#' )
#'
#' ## Now neat the output table ----
#' mw_neat_output_wfhz(df = pl)
#'
#'
#' @export
mw_neat_output_wfhz <- function(df) {

  ## Check if `df` is grouped ----
  is_grouped <- is_grouped_df(df)

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
    dps_wgt = round(.data$dps_wgt),
    dps_hgt = round(.data$dps_hgt),
    skew = round(.data$skew, digits = 2),
    kurt = round(.data$kurt, digits = 2)
  ) |>
  ## Rename columns ----
setNames(
  c( if (is_grouped) "Group" else NULL,
    "Total children", "Flagged data (%)", "Class. of flagged data",
    "Sex ratio (p)", "Class. of sex ratio", "Age ratio (p)",
    "Class. of age ratio", "DPS weight (#)", "Class. DPS weight",
    "DPS height (#)", "Class. DPS height", "Standard Dev* (#)",
    "Class. of standard dev", "Skewness* (#)", "Class. of skewness",
    "Kurtosis* (#)", "Class. of kurtosis", "Overall score", "Overall quality"
  )
)
## Return data frame ----
df
}

