#'
#' Wrangle MUAC data
#'
#' @description
#' Calculate z-scores for MUAC-for-age (MFAZ) and identify outliers based on
#' the SMART methodology. When age is not supplied, wrangling will consist only
#' in detecting outliers from raw the MUAC values.
#'
#' @param df A dataset object of class `data.frame` to wrangle data from.
#'
#' @param sex A `numeric` or `character` vector of child's sex. Code values should
#' only be 1 or "m" for males and 2 or "f" for females. Make sure sex values
#' are coded in either of the aforementioned before to call the function. If input
#' codes are different than expected, the function will stop execution and
#' return an error message with the type of mismatch.
#'
#' @param .recode_sex Logical. Set to `TRUE` if the values for `sex` are not coded
#' as 1 (for males) or 2 (for females). Otherwise, set to `FALSE` (default).
#'
#' @param age A vector of class `numeric` of child's age in months.
#'
#' @param muac A vector of class `numeric` of child's age in months. If the class
#' is different than expected, the function will stop execution and return an error
#' message indicating the type of mismatch.
#'
#' @param .recode_muac Logical. Set to `TRUE` if the values for raw MUAC should be
#' converted to either centimeters or millimeters. Otherwise, set to `FALSE`
#' (default)
#'
#' @param .to A choice of the unit to which the MUAC values should be converted;
#' "cm" for centimeters, "mm" for millimeters and "none" to leave as it is.
#'
#' @param .decimals The number of decimals places the z-scores should have.
#' Default is 3.
#'
#' @returns A data frame based on `df`. New variables named `mfaz` and
#' `flag_mfaz`, of child's MFAZ and detected outliers, will be created. When age
#' is not supplied, only `flag_muac` variable is created. This refers to outliers
#' detected based on the raw MUAC values.
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
#' @seealso
#' [flag_outliers()] [remove_flags()] [mw_wrangle_age()]
#'
#'
#' @examples
#' ## When age is available ----
#' anthro.02 |>
#'   mw_wrangle_age(
#'     dos = NULL,
#'     dob = NULL,
#'     age = age,
#'     .decimals = 2
#'   ) |>
#'   mw_wrangle_muac(
#'     sex = sex,
#'     age = age,
#'     muac = muac,
#'     .recode_sex = TRUE,
#'     .recode_muac = TRUE,
#'     .to = "cm",
#'     .decimals = 3
#'   )
#'
#' ## When age is not available ----
#' anthro.02 |>
#'   mw_wrangle_muac(
#'     sex = sex,
#'     age = NULL,
#'     muac = muac,
#'     .recode_sex = TRUE,
#'     .recode_muac = TRUE,
#'     .to = "cm",
#'     .decimals = 3
#'   )
#'
#' @export
#'
mw_wrangle_muac <- function(df,
                            sex,
                            muac,
                            age = NULL,
                            .recode_sex = TRUE,
                            .recode_muac = TRUE,
                            .to = c("cm", "mm", "none"),
                            .decimals = 3) {

  ## Enforce options in argument .to ----
  .to <- match.arg(.to)

  ## Difuse sex variable for NSE----
  sex <- eval_tidy(enquo(sex), df)

  ## Check if vector of sex is coded in either "m" and "f" or 1 and 2 ----
  x <- as.factor(as.character(sex))
  if(!(all(levels(x) %in% c("m", "f")) | all(levels(x) %in% c("1", "2")))) {
    stop("Values for sex should either be 'm', 'f' or 1 and 2 for male and female respectively")
  }

  ## Capture expressions to evaluate later ----
  recode_sex <- quote(
    if (.recode_sex) {
      sex <- ifelse({{ sex }} == "m", 1, 2)
    } else {{{ sex }}}
  )

  ## Capture expressions to evaluate later ----
  rec_muac <- quote(
    if (.recode_muac && .to == "cm") {
      muac <- recode_muac({{ muac }}, .to = "cm")
    } else if (.recode_muac && .to == "mm") {
      muac <- recode_muac({{ muac }}, .to = "mm")
    } else {{{ muac }}}
  )

  ## Difuse arguments for NSE ----
  age <- eval_tidy(enquo(age), df)

  if (!is.null(age)) {
    ## Calculate z-scores and identify outliers on MFAZ ----
    df <- df |>
      mutate(
        muac = !!rec_muac,
        sex = !!recode_sex,
      ) |>
      addWGSR(
        sex = {{ "sex" }},
        firstPart = {{ "muac" }},
        secondPart = "age_days",
        index = "mfa",
        digits = .decimals
      ) |>
      mutate(
        flag_mfaz = do.call(flag_outliers, list(.data$mfaz, .from = "zscores"))
      )
  } else {
    ## Identify outliers on raw MUAC values ----
    df <- df |>
      mutate(
        sex = !!recode_sex,
        flag_muac = do.call(flag_outliers, list({{ muac }}, .from = "raw_muac"))
      )
  }
  ## Return ----
  as_tibble(df)
}
