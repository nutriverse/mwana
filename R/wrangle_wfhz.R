#'
#' Wrangle weight-for-height data
#'
#' @description
#' Calculate z-scores for weight-for-height (WFHZ) and identify outliers based
#' on the SMART methodology.
#'
#' @param df A `data.frame` object to wrangle data from.
#'
#' @param sex A `numeric` or `character` vector of child's sex. Code values
#' should only be 1 or "m" for males and 2 or "f" for females.
#'
#' @param .recode_sex Logical. Set to TRUE if the values for `sex` are not coded
#' as 1 (for males) or 2 (for females). Otherwise, set to FALSE (default).
#'
#' @param weight A vector of class `double` of child's weight in kilograms.
#'
#' @param height A vector of class `double` of child's height in centimeters.
#'
#' @param .decimals The number of decimal places to use for z-score outputs.
#' Default is 3.
#'
#' @returns A data frame based on `df` with new variables named `wfhz` for
#' child's WFHZ and `flag_wfhz` for detected outliers added.
#'
#' @references
#' SMART Initiative (2017). *Standardized Monitoring and Assessment for Relief*
#' *and Transition*. Manual 2.0. Available at: <https://smartmethodology.org>.
#'
#' @seealso
#' [flag_outliers()] [remove_flags()]
#'
#' @examples
#' mw_wrangle_wfhz(
#'   df = anthro.01,
#'   sex = sex,
#'   weight = weight,
#'   height = height,
#'   .recode_sex = TRUE,
#'   .decimals = 2
#' )
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
  if (!is.double(weight)) {
    stop(
      "`weight` must be of class double not ",
      class(weight), ". Please try again."
    )
  }

  ## Check if the class of vector height is "double" ----
  if (!is.double(height)) {
    stop(
      "`height` must be of class double not ",
      class(height), ". Please try again."
    )
  }

  ## Difuse sex variable for NSE----
  sex <- rlang::eval_tidy(enquo(sex), df)

  ## Enforce code value of `sex` ----
  x <- as.factor(as.character(sex))
  if (!(all(levels(x) %in% c("m", "f")) | all(levels(x) %in% c("1", "2")))) {
    stop(
      'Values for sex should either be "m" and "f" or 1 and 2 for male and female respectively'
    )
  }

  ## Capture expressions to evaluate later ----
  recode_sex <- quote(
    if (.recode_sex) {
      sex <- ifelse({{ sex }} == "m", 1, 2)
    } else {{{ sex }}}
  )

  ## Compute z-scores ----
  df <- dplyr::mutate(
    .data = df,
    sex = !!recode_sex,
    weight = ifelse(weight < 3 | weight > 31, NA, weight),
    height = ifelse(height < 54 | height > 124, NA, height)
  ) |>
    zscorer::addWGSR(
      sex = "sex",
      firstPart = "weight",
      secondPart = "height",
      index = "wfh",
      digits = .decimals
    ) |>
    ## Identify and flag outliers ----
    dplyr::mutate(
      flag_wfhz = do.call(flag_outliers, list(.data$wfhz, .from = "zscores"))
    )

  ## Return df ---
  tibble::as_tibble(df)
}
