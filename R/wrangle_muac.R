#'
#' Wrangle MUAC data
#'
#' @description
#' Calculate z-scores for MUAC-for-age (MFAZ) and identify outliers based on
#' the SMART methodology. When age is not supplied, only outliers are detected 
#' from the raw MUAC values. The function only works after age has gone through
#' [mw_wrangle_age()].
#'
#' @param df A `data.frame` object to wrangle data from.
#'
#' @param sex A `numeric` or `character` vector of child's sex. Code values 
#' should only be 1 or "m" for males and 2 or "f" for females.
#'
#' @param .recode_sex Logical. Set to TRUE if the values for `sex` are not coded
#' as 1 (for males) or 2 (for females). Otherwise, set to FALSE (default).
#'
#' @param age A `numeric` vector of child's age in months. Default is NULL.
#'
#' @param muac A `numeric` vector of child's age in months.
#'
#' @param .recode_muac Logical. Set to TRUE if the values for raw MUAC should be
#' converted to either centimeters or millimeters. Otherwise, set to FALSE
#' (default)
#'
#' @param .to A choice of the measuring unit to convert MUAC values into. Can be 
#' "cm" for centimeters, "mm" for millimeters, or "none" to leave as it is.
#'
#' @param .decimals The number of decimal places to use for z-score outputs.
#' Default is 3.
#'
#' @returns A `tibble` based on `df`. If `age = NULL`, `flag_muac` variable for
#' detected MUAC outliers based on raw MUAC is added to `df`. Otherwise, 
#' variables named `mfaz` for child's MFAZ and `flag_mfaz` for detected outliers
#' based on SMART guidelines are added to `df`.
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
#' ## When age is available, wrangle it first before calling the function ----
#' w <- mw_wrangle_age(
#'   df = anthro.02,
#'   dos = NULL,
#'   dob = NULL,
#'   age = age,
#'   .decimals = 2
#' )
#'
#' ### Then apply the function to wrangle MUAC data ----
#' mw_wrangle_muac(
#'   df = w,
#'   sex = sex,
#'   age = age,
#'   muac = muac,
#'   .recode_sex = TRUE,
#'   .recode_muac = TRUE,
#'   .to = "cm",
#'   .decimals = 3
#' )
#'
#' ## When age is not available ----
#' mw_wrangle_muac(
#'   df = anthro.02,
#'   sex = sex,
#'   age = NULL,
#'   muac = muac,
#'   .recode_sex = TRUE,
#'   .recode_muac = TRUE,
#'   .to = "cm",
#'   .decimals = 3
#' )
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

  ## Enforce options in `.to` ----
  .to <- match.arg(.to)

  ## Defuse sex variable for NSE----
  sex <- rlang::eval_tidy(enquo(sex), df)

  ## Enforce code values in `sex` ----
  x <- as.factor(as.character(sex))
  if (!(all(levels(x) %in% c("m", "f")) | all(levels(x) %in% c("1", "2")))) {
    stop(
      'Values for sex should either be "m" and "f" or 1 and 2 for male and female respectively')
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

  ## Defuse arguments for NSE ----
  age <- rlang::eval_tidy(enquo(age), df)

  if (!is.null(age)) {
    ## Calculate z-scores and identify outliers on MFAZ ----
    df <- dplyr::mutate(
      .data = df,
      muac = !!rec_muac,
      sex = !!recode_sex,
    ) |>
      zscorer::addWGSR(
        sex = {{ "sex" }},
        firstPart = {{ "muac" }},
        secondPart = "age_days",
        index = "mfa",
        digits = .decimals
      ) |>
      dplyr::mutate(
        flag_mfaz = do.call(flag_outliers, list(.data$mfaz, .from = "zscores"))
      )
  } else {
    ## Identify outliers on raw MUAC values ----
    df <- dplyr::mutate(
      .data = df,
      sex = !!recode_sex,
      flag_muac = do.call(flag_outliers, list({{ muac }}, .from = "raw_muac"))
    )
  }

  ## Return df ----
  tibble::as_tibble(df)
}
