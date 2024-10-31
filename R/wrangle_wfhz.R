#'
#' Wrangle weight-for-height data
#'
#' @description
#' Calculate z-scores for weight-for-height (WFHZ) and identify outliers based on
#' the SMART methodology.
#'
#' @param df A dataset object of class `data.frame` to wrangle data from.
#'
#' @param sex A `numeric` or `character` vector of child's sex. Code values should
#' only be 1 or "m" for males and 2 or "f" for females. Make sure sex values
#' are coded in either of the aforementioned before to call the function. If input
#' codes are neither of the above, the function will stop execution and
#' return an error message with the type of mismatch.
#'
#' @param .recode_sex Logical. Set to `TRUE` if the values for `sex` are not coded
#' as 1 (for males) or 2 (for females). Otherwise, set to `FALSE` (default).
#'
#' @param weight A vector of class `double` of child's weight in kilograms. If the input
#' is of a different class, the function will stop execution and return an error
#' message indicating the type of mismatch.
#'
#' @param height A vector of class `double` of child's height in centimeters. If the input
#' is of a different class, the function will stop execution and return an error
#' message indicating the type of mismatch.
#'
#' @param .decimals The number of decimals places the z-scores should have.
#' Default is 3.
#'
#' @returns A data frame based on `df`. New variables named `wfhz` and
#' `flag_wfhz`, of child's WFHZ and detected outliers, will be created.
#'
#' @references
#' SMART Initiative (2017). *Standardized Monitoring and Assessment for Relief
#' and Transition*. Manual 2.0. Available at: <https://smartmethodology.org>.
#'
#' @seealso
#' [flag_outliers()] [remove_flags()]
#'
#' @examples
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
    stop("`weight` must be of class 'double'; not ", shQuote(class(weight)), ". Please try again.")
  }

  ## Check if the class of vector height is "double" ----
  if(!is.double(height)) {
    stop("`height` must be of class 'double'; not ", shQuote(class(height)), ". Please try again.")
  }

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
