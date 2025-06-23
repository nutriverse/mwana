#'
#' Check the plausibility and acceptability of raw MUAC data
#'
#' @description
#' Check the overall plausibility and acceptability of raw MUAC data
#' through a structured test suite encompassing checks for sampling and
#' measurement-related biases in the dataset. The test suite in this function
#' follows the recommendation made by Bilukha & Kianian (2023).
#'
#' @param df A `data.frame` object to check. It must have been wrangled using
#' the [mw_wrangle_muac()] function.
#'
#' @param sex A `numeric` vector for child's sex with 1 = males and 2 = females.
#'
#' @param muac A vector of class `double` of child's MUAC in centimeters.
#'
#' @param flags A `numeric` vector of flagged records.
#'
#' @param .by A `character` or `numeric` vector of the geographical areas for
#' where the data was collected and for which the analysis should be summarised
#' for.
#'
#' @returns A single row summary `tibble` with 9 columns (if ungrouped analysis, 
#' otherwise 10), containing the plausibility check results and their respective 
#' acceptability ratings.
#'
#' @details
#' Cut-off points used for the percent of flagged records:
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
#' @seealso [mw_wrangle_muac()] [flag_outliers()]
#'
#' @examples
#' ## First wrangle MUAC data ----
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
mw_plausibility_check_muac <- function(df, sex, muac, flags, .by = NULL) {
  ## Difuse argument `.by` ----
  .by <- enquo(.by)

  if (rlang::quo_is_null(.by)) {
    ## Summarise statistics  ----
    df <- dplyr::summarise(
      .data = df,
      n = dplyr::n(),
      flagged = sum({{ flags }}, na.rm = TRUE) / n(),
      flagged_class = rate_propof_flagged(.data$flagged, .in = "raw_muac"),
      sex_ratio = nipnTK::sexRatioTest({{ sex }}, codes = c(1, 2))[["p"]],
      sex_ratio_class = rate_agesex_ratio(.data$sex_ratio),
      dps = nipnTK::digitPreference(
        {{ muac }},
        digits = 0, values = 0:9
      )[["dps"]],
      dps_class = nipnTK::digitPreference(
        {{ muac }},
        digits = 0, values = 0:9
      )[["dpsClass"]],
      sd = stats::sd(remove_flags({{ muac }}, .from = "raw_muac"), na.rm = TRUE),
      sd_class = rate_std(.data$sd, .of = "raw_muac")
    )
  } else {
    ## Summarise statistics  ----
    df <- dplyr::summarise(
      .data = df,
      n = dplyr::n(),
      flagged = sum({{ flags }}, na.rm = TRUE) / n(),
      flagged_class = rate_propof_flagged(.data$flagged, .in = "raw_muac"),
      sex_ratio = nipnTK::sexRatioTest({{ sex }}, codes = c(1, 2))[["p"]],
      sex_ratio_class = rate_agesex_ratio(.data$sex_ratio),
      dps = nipnTK::digitPreference(
        {{ muac }},
        digits = 0, values = 0:9
      )[["dps"]],
      dps_class = nipnTK::digitPreference(
        {{ muac }},
        digits = 0, values = 0:9
      )[["dpsClass"]],
      sd = stats::sd(remove_flags({{ muac }}, .from = "raw_muac"), na.rm = TRUE),
      sd_class = rate_std(.data$sd, .of = "raw_muac"),
      .by = !!.by
    )
  }

  ## Return data.frame ----
  df
}



#'
#' Clean and format the output tibble returned from the MUAC plausibility check
#'
#' @description
#' Converts scientific notations to standard notations, rounds off values, and
#' renames columns to meaningful names.
#'
#' @param df A `tibble` object returned by the [mw_plausibility_check_muac()]
#' function containing the summarized results to be formatted.
#'
#' @param .by A `character` or `numeric` vector of the geographical areas for
#' where the data was collected and for which the analysis should be summarised
#' for.
#'
#' @returns
#' A `data.frame` object of the same length and width as `df`, with column names
#' and values formatted for clarity and readability.
#'
#' @examples
#' ## First wrangle MUAC data ----
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
mw_neat_output_muac <- function(df, .by = NULL) {
  ## Difuse argument ----
  .by <- enquo(.by)

  if (rlang::quo_is_null(.by)) {
    ## Format data frame ----
    df <- dplyr::mutate(
      .data = df,
      flagged = scales::label_percent(
        accuracy = 0.1, suffix = "%", decimal.mark = "."
      )(.data$flagged),
      sex_ratio = scales::label_pvalue()(.data$sex_ratio),
      sd = round(.data$sd, digits = 2),
      dps = round(.data$dps)
    ) |>
      ## Rename columns ----
      stats::setNames(
        c(
          "Total children", "Flagged data (%)", "Class. of flagged data",
          "Sex ratio (p)", "Class. of sex ratio", "DPS(#)", "Class. of DPS",
          "Standard Dev* (#)", "Class. of standard dev"
        )
      )
  } else {
    ## Format data frame ----
    df <- dplyr::mutate(
      .data = df,
      flagged = scales::label_percent(
        accuracy = 0.1, suffix = "%", decimal.mark = "."
      )(.data$flagged),
      sex_ratio = scales::label_pvalue()(.data$sex_ratio),
      sd = round(.data$sd, digits = 2),
      dps = round(.data$dps)
    ) |>
      ## Rename columns ----
      stats::setNames(
        c(
          "Group", "Total children", "Flagged data (%)", "Class. of flagged data",
          "Sex ratio (p)", "Class. of sex ratio", "DPS(#)", "Class. of DPS",
          "Standard Dev* (#)", "Class. of standard dev"
        )
      )
  }

  ## Return data.frame ----
  df
}
