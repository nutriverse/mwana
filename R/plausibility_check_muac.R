#'
#' Check the plausibility and acceptability of raw MUAC data
#'
#' @description
#' Check the overall plausibility and acceptability of raw MUAC data through a
#' structured test suite encompassing sampling and measurement-related biases checks
#' in the dataset. The test suite in this function follows the recommendation made
#' by Bilukha, O., & Kianian, B. (2023).
#'
#' @param df An object of class `data.frame` to check. It must have been
#' wrangled using this package's wrangling function for MUAC.
#'
#' @param sex A vector of class `numeric` of child's sex.
#'
#' @param muac A vector of class `double` of child's MUAC in centimeters.
#'
#' @param flags A vector of class `numeric` of flagged records.
#'
#' @returns A summarized table of class `data.frame`, of length 9 and width 1, for
#' the plausibility test results and their respective acceptability ratings.
#'
#' @details
#' Cut-off points used for the percent of flagged records:
#' |**Excellent** | **Good** | **Acceptable** | **Problematic** |
#' | :---: | :---: | :---: | :---: |
#' | 0.0 - 1.0 | >1.0 - 1.5| >1.5 - 2.0 | >2.0  |
#'
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
#' @seealso [mw_wrangle_muac()] [flag_outliers()]
#'
#' @examples
#' ## First wranlge MUAC data ----
#' df_muac <- mw_wrangle_muac(
#'   df = anthro.01,
#'   sex = sex,
#'   muac = muac,
#'   age = NULL,
#'   .recode_sex = TRUE,
#'   .recode_muac = FALSE,
#'   .to = "none"
#' )
#'
#' ## Then run the plausibility check ----
#' mw_plausibility_check_muac(
#'   df = df_muac,
#'   flags = flag_muac,
#'   sex = sex,
#'   muac = muac
#' )
#'
#' @export
#'
mw_plausibility_check_muac <- function(df, sex, muac, flags) {
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



#'
#'
#'
#' Clean and format the output table returned from the MUAC plausibility check
#' for improved clarity and readability.
#'
#' @description
#' Clean and format the output table returned from the plausibility check of raw
#' MUAC data for improved clarity and readability. It converts scientific notations
#' to standard notations, round values and rename columns to meaningful names.
#'
#' @param df An object of class `data.frame` returned by this package's
#' plausibility checker for raw MUAC data, containing the summarized results to be
#' formatted.
#'
#' @returns
#' A `data.frame` object of the same length and width as `df`, with column names and
#' values formatted for clarity and readability.
#'
#' @examples
#' ## First wranlge MUAC data ----
#' df_muac <- mw_wrangle_muac(
#'   df = anthro.01,
#'   sex = sex,
#'   muac = muac,
#'   age = NULL,
#'   .recode_sex = TRUE,
#'   .recode_muac = FALSE,
#'   .to = "none"
#' )
#'
#' ## Then run the plausibility check ----
#' pl_muac <- mw_plausibility_check_muac(
#'   df = df_muac,
#'   flags = flag_muac,
#'   sex = sex,
#'   muac = muac
#' )
#'
#' ## Neat the output table ----
#'
#' mw_neat_output_muac(df = pl_muac)
#'
#' @export
#'
mw_neat_output_muac <- function(df) {

  ## Format data frame ----
  df <- df |>
    mutate(
      flagged = .data$flagged |>
        label_percent(accuracy = 0.1, suffix = "%", decimal.mark = ".")(),
      sex_ratio = .data$sex_ratio  |>  scales::label_pvalue()(),
      sd = round(.data$sd, digits = 2),
      dps = round(.data$dps)
    ) |>
    ## Rename columns ----
  setNames(
    c("Total children", "Flagged data (%)", "Class. of flagged data", "Sex ratio (p)",
      "Class. of sex ratio", "DPS(#)", "Class. of DPS", "Standard Dev* (#)",
      "Class. of standard dev")
  )
  ## Return data frame ----
  df
}
