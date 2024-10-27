#'
#'
#' Wrangle weight-for-height and MUAC data
#'
#' @description
#' This function performs data wrangling by calculating the weight-for-height
#' and the MUAC-for-age z-scores, followed by the detection and flagging of outliers.
#' For MUAC data, if age is not supplied, z-scores do not get computed. In such
#' cases, outlier detection and flagging will be based on the absolute MUAC values.
#'
#' @param df A dataset of class `data.frame` to wrangle data from.
#'
#' @param sex A `numeric` or `character` vector of child's sex. Code values should
#' be 1 or "m" for boy and 2 or "f" for girl. The variable name must be sex,
#' otherwise it will not work.
#'
#' @param .recode_sex Logical. Default is `FALSE`. Setting to `TRUE` assumes that
#' the sex variable is a character vector of values "m" for boys and "f" for girls
#' and will recode them to 1 and 2 respectively.
#'
#' @param age A vector of class `numeric` of child's age in months.
#'
#' @param muac A vector of class `numeric` of child's age in months.
#'
#' @param .recode_muac Logical. Default is `FALSE`. Set to `TRUE` if MUAC values
#' should be converted to either centimeters or millimeters.
#'
#' @param .to A choice of the unit to which the MUAC values should be converted.
#' "cm" for centimeters, "mm" for millimeters and "none" to leave as it is.
#'
#' @param weight A vector of class `double` of child's weight in kilograms.
#'
#' @param height A vector of class `double` of child's height in centimeters.
#'
#' @param .decimals The number of decimals places the z-scores should have.
#' Default is 3.
#'
#' @returns A data frame based on `df`. New variables named `wfhz` and
#' `flag_wfhz`, of child's weight-for-height z-scores and flags, or `mfaz` and
#' `flag_mfaz`, of child's MUAC-for-age z-scores and flags, will be created. For
#' MUAC, when age is not supplied only `flag_muac` variable is created.
#' This refers to flags based on the absolute MUAC values as recommended by
#' [Bilukha, O., & Kianian, B. (2023).](https://doi.org/10.1111/mcn.13478).
#'
#' @details
#' The flagging criterion used for the WFHZ and MFAZ is as in
#' [SMART plausibility check](https://smartmethodology.org/). A fixed flagging
#' criterion is used for the absolute MUAC values. This is as recommended by
#' [Bilukha, O., & Kianian, B. (2023).](https://doi.org/10.1111/mcn.13478)
#'
#' @examples
#'
#' ## An example application of `mw_wrangle_wfhz()` ----
#' anthro.01 |>
#'   mw_wrangle_wfhz(
#'     sex = sex,
#'     weight = weight,
#'     height = height,
#'     .recode_sex = TRUE,
#'     .decimals = 2
#'   )
#'
#' ## An example application of `mw_wrangle_muac()` ----
#' ### Sample data ----
#' df <- data.frame(
#'   survey_date = as.Date(c(
#'     "2023-01-01", "2023-01-01", "2023-01-01", "2023-01-01", "2023-01-01"
#'   )),
#'   birth_date = as.Date(c(
#'     "2019-01-01", NA, "2018-03-20", "2019-11-05", "2021-04-25"
#'   )),
#'   age = c(NA, 36, NA, NA, NA),
#'   sex = c("m", "f", "m", "m", "f"),
#'   muac = c(110, 130, 300, 123, 125)
#' )
#'
#' ### The application of the function ----
#' df |>
#'   mw_wrangle_age(
#'     dos = survey_date,
#'     dob = birth_date,
#'     age = age,
#'     .decimals = 2
#'   ) |>
#'   mw_wrangle_muac(
#'     sex = sex,
#'     age = "age",
#'     muac = muac,
#'     .recode_sex = TRUE,
#'     .recode_muac = TRUE,
#'     .to = "cm",
#'     .decimals = 3
#'   )
#'
#' @rdname wrangler
#'
#' @export
#'

mw_wrangle_wfhz <- function(df,
                            sex,
                            weight,
                            height,
                            .recode_sex = TRUE,
                            .decimals = 3) {

  ## Difuse arguments to be evaluated later ----
  weight <- rlang::eval_tidy(rlang::enquo(weight), df)
  height <- rlang::eval_tidy(rlang::enquo(height), df)

  ## Check if the class of vector weight is "double" ----
  if(!is.double(weight)) {
    stop("Weight should be of class 'double'. Please try again")
  }

  ## Check if the class of vector height is "double" ----
  if(!is.double(height)) {
    stop("Height should be of class 'double'. Please try again")
  }

  ## Capture expressions to evaluate later ----
  recode_sex <- quote(
    if (.recode_sex) {
      sex <- ifelse({{ sex }} == "m", 1, 2)
    } else {{{ sex }}}
  )

  ## Compute z-scores ----
  df <- df |>
    mutate(
      sex = !!recode_sex
    ) |>
    addWGSR(
      sex = "sex",
      firstPart = "weight",
      secondPart = "height",
      index = "wfh",
      digits = .decimals
    ) |>
    ## Identify and flag outliers ----
    mutate(
      flag_wfhz = do.call(flag_outliers, list(.data$wfhz, .from = "zscores"))
    )
  ## Return ---
  tibble::as_tibble(df)
}
