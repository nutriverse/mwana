#'
#'
#' Wrangle weight-for-height data
#'
#' @description
#' This function performs data wrangling by calculating the weight-for-height
#' followed by the detection and flagging of outliers based on the SMART flagging
#' criteria.
#'
#' @param df A dataset of class `data.frame` to wrangle data from.
#'
#' @param sex A `numeric` or `character` vector of child's sex. Code values should
#' be 1 or "m" for boy and 2 or "f" for girl.
#'
#' @param .recode_sex Logical. Set to `TRUE` if the values for `sex` are not coded
#' as 1 (for males) or 2 (for females). Otherwise, set to `FALSE` (default).
#'
#' @param weight A vector of class `double` of child's weight in kilograms.
#'
#' @param height A vector of class `double` of child's height in centimeters.
#'
#' @param .decimals The number of decimals places the z-scores should have.
#' Default is 3.
#'
#' @returns A data frame based on `df`. New variables named `wfhz` and
#' `flag_wfhz`, of child's weight-for-height z-scores and flags.
#'
#' @details
#' The flagging criterion used for the WFHZ is as in
#' [SMART plausibility check](https://smartmethodology.org/).
#'
#' @examples
#'
#' anthro.01 |>
#'   mw_wrangle_wfhz(
#'     sex = sex,
#'     weight = weight,
#'     height = height,
#'     .recode_sex = TRUE,
#'     .decimals = 2
#'   )
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
  weight <- eval_tidy(enquo(weight), df)
  height <- eval_tidy(enquo(height), df)

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
  as_tibble(df)
}
