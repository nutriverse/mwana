#'
#' Wrangle MUAC data
#'
#' @description
#' This function performs data wrangling by calculating the MUAC-for-age z-scores,
#' followed by the detection and flagging of outliers based on the SMART flagging
#' criteria. When age is not supplied, z-scores do not get computed. Instead,
#' outlier detection and flagging is based on the raw MUAC values.
#'
#' @param df A dataset of class `data.frame` to wrangle data from.
#'
#' @param sex A `numeric` or `character` vector of child's sex. Code values should
#' be 1 or "m" for boy and 2 or "f" for girl.
#'
#' @param .recode_sex Logical. Set to `TRUE` if the values for `sex` are not coded
#' as 1 (for males) or 2 (for females). Otherwise, set to `FALSE` (default).
#'
#' @param age A vector of class `numeric` of child's age in months.
#'
#' @param muac A vector of class `numeric` of child's age in months.
#'
#' @param .recode_muac Logical. Set to `TRUE` if the raw MUAC values should be
#' converted to either centimeters or millimeters. Otherwise, set to `FALSE`
#' (default)
#'
#' @param .to A choice of the unit to which the MUAC values should be converted.
#' "cm" for centimeters, "mm" for millimeters and "none" to leave as it is.
#'
#' @param .decimals The number of decimals places the z-scores should have.
#' Default is 3.
#'
#' @returns A data frame based on `df`. New variables named `mfaz` and
#' `flag_mfaz`, of child's MUAC-for-age z-scores and flags will be created. For
#' MUAC, when age is not supplied only `flag_muac` variable is created.
#' This refers to flags based on the raw MUAC values as recommended by
#' [Bilukha, O., & Kianian, B. (2023).](https://doi.org/10.1111/mcn.13478).
#'
#' @details
#' A fixed flagging criterion is used for the raw MUAC values. This is as
#' recommended by
#' [Bilukha, O., & Kianian, B. (2023).](https://doi.org/10.1111/mcn.13478)
#'
#' @examples
#'
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

  ## Difuse arguments to be evaluated later ----
  age <- eval_tidy(enquo(age), df)

  if (!is.null(age)) {
    ## Compute z-scores and identify flags on MFAZ ----
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
    ## Identify flags on the raw MUAC values ----
    df <- df |>
      mutate(
        sex = !!recode_sex,
        flag_muac = do.call(flag_outliers, list({{ muac }}, .from = "raw_muac"))
      )
  }
  ## Return ----
  as_tibble(df)
}
