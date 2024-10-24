#'
#'
#' Identify and flag outliers
#'
#' @description
#' Outliers are extreme values that deviate remarkably from the survey mean, making
#' them unlikely to be accurate measurements. This function detects and signals
#' them based on a criterion set for the WFHZ, the MFAZ and for the absolute MUAC
#' values.
#'
#' @param x A vector of class `double` of WFHZ or MFAZ or absolute MUAC values.
#' The latter should be in millimeters.
#'
#' @param type A choice between `zscore` and `crude` for where outliers should be
#' detected and flagged from.
#'
#' @param unit A choice between `zscore` and `crude` for where outliers should be
#' detected and flagged from.
#'
#' @return A vector of the same length as `x` of flagged observations that are
#' outliers: 1 for is a flag and 0 is not a flag.
#'
#' @details
#' The flagging criterion used for the WFHZ and the MFAZ is as in
#' [SMART plausibility check](https://smartmethodology.org/). A fixed flagging
#' criterion is used for the absolute MUAC values. This is as recommended by
#' [Bilukha, O., & Kianian, B. (2023).](https://doi.org/10.1111/mcn.13478)
#'
#'
#' @examples
#'
#' ## Sample data for absolute MUAC values ----
#' x <- anthro.01$muac
#'
#' ## Apply the function with type set to "crude" ----
#' flag_outliers(x, type = "crude")
#'
#' ## Sample data for MFAZ or for WFHZ values ----
#' x <- anthro.02$mfaz
#'
#' # Apply the function with type set to "zscore" ----
#' flag_outliers(x, type = "zscore")
#'
#' @rdname outliers
#' @export
#'
flag_outliers <- function(x, type = c("zscore", "crude")) {
  type <- match.arg(type)

  if (type == "zscore") {
    mean_zscore <- mean(x, na.rm = TRUE)
    flags <- ifelse((x < (mean_zscore - 3) | x > (mean_zscore + 3)), 1, 0)
    flags <- ifelse(is.na(x), NA, flags)
    flags

  } else {
    flags <- ifelse(x < 100 | x > 200, 1, 0)
    flags <- ifelse(is.na(x), NA, flags)
    flags
  }
}


#'
#'
#' Remove outliers
#'
#' @rdname outliers
#'
remove_flags <- function(x, unit = c("zscore", "crude")) {

  ## Match arguments ----
  unit <- match.arg(unit)

  ## Control flow based on unit ----
  switch(
    unit,
    ### Remove flags when unit = "zscore" ----
    "zscore" = {
      mean_x <- mean(x, na.rm = TRUE)
      zs <- ifelse((x < (mean_x - 3) | x > (mean_x + 3)) | is.na(x), NA_real_, x)
    },
    ### Remove flags when unit = "crude" ----
    "crude" = {
      cr <- ifelse(x < 100 | x > 200 | is.na(x), NA_integer_, x)
    }
  )
}


#'
#'
#'
#' Convert MUAC values to either centimeters or millimeters
#'
#' @description
#' Recode the MUAC values to either centimeters or millimeters as required.
#'
#' @param muac A vector of class `double` or `integer` of the absolute MUAC values.
#'
#' @param unit A choice of the unit to which the MUAC values should be converted.
#'
#' @returns A numeric vector of the same length `muac`, with values converted
#' to the chosen unit.
#'
#' @examples
#'
#' ## Recode from millimeters to centimeters ----
#' muac <- anthro.01$muac
#' muac_cm <- recode_muac(muac, unit = "cm")
#'
#' ## Using the `muac_cm` object to recode it back to "mm" ----
#' muac_mm <- recode_muac(muac_cm, unit = "mm")
#'
#' @export
#'
recode_muac <- function(muac, unit = c("cm", "mm")) {

  ## Check if unit's arguments match ----
  stopifnot(unit %in% c("cm", "mm"))

  ## Recode muac conditionally ----
  switch(
    unit,
    ### Recode to millimeters ----
    "mm" = {muac <- muac * 10},
    ### Recode to centimeters ----
    "cm" = {muac <- muac / 10},
    stop("Invalid 'units' argument. Please choose either 'cm' or 'mm'.")
  )
}


#'
#'
#' Wrangle weight-for-height and MUAC data
#'
#' @description
#' This function performs data wrangling by calculating weight-for-height
#' and MUAC-for-age z-scores, followed by the detection and flagging of outliers.
#' For MUAC data, if age is not supplies, z-scores do not get computed. In such
#' cases, outlier detection and flagging are based on the absolute MUAC values.
#'
#' @param df A dataset of class `data.frame` to wrangle data from.
#'
#' @param sex A numeric or character vector of child's sex. Code values should
#' be 1 or "m" for boy and 2 or "f" for girl. The variable name must be sex,
#' otherwise it will not work.
#'
#' @param .recode_sex Logical. Default is `FALSE`. Setting to `TRUE` assumes that
#' the sex variable is a character vector of values "m" for boys and "f" for girls
#' and will recode them to 1 and 2 respectively.
#'
#' @param muac A vector of class `double` or `integer` of the absolute MUAC values.
#'
#' @param .recode_muac Logical. Default is `FALSE`. Set to `TRUE` if MUAC values
#' should be converted to either centimeters or millimeters.
#'
#' @param unit A choice of the unit to which the MUAC values should be converted.
#' "cm" for centimeters, "mm" for millimeters and "none" to leave as it is.
#'
#' @param age A double vector of child's age in months. It must be named age,
#' otherwise it will not work.
#'
#' @param weight A vector of class `double` of child's weight in kilograms.
#'
#' @param height A vector of class `double` of child's height in centimeters.
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
#' ## An example application of `process_wfhz_data()` ----
#'
#' anthro.01 |>
#' process_wfhz_data(
#' sex = sex,
#' weight = weight,
#' height = height,
#' .recode_sex = TRUE
#' )
#'
#' ## An example application of `process_muac_data()` ----
#'
#' ### Sample data ----
#' df <- data.frame(
#'  survey_date = as.Date(c(
#'  "2023-01-01", "2023-01-01", "2023-01-01", "2023-01-01", "2023-01-01")),
#'  birth_date = as.Date(c(
#'  "2019-01-01", NA, "2018-03-20", "2019-11-05", "2021-04-25")),
#'  age = c(NA, 36, NA, NA, NA),
#'  sex = c("m", "f", "m", "m", "f"),
#'  muac = c(110, 130, 300, 123, 125)
#'  )
#'
#'  ### The application of the function ----
#'
#'  df |>
#'  mw_wrangle_age(
#'  dos = survey_date,
#'  dob = birth_date,
#'  age = age
#'  ) |>
#'  process_muac_data(
#'  sex = sex,
#'  age = "age",
#'  muac = muac,
#'  .recode_sex = TRUE,
#'  .recode_muac = TRUE,
#'  unit = "cm"
#'  )
#'
#' @rdname wrangler
#'
#' @export
#'

process_wfhz_data <- function(df,
                              sex,
                              weight,
                              height,
                              .recode_sex = TRUE) {

  recode_sex <- quote(
    if (.recode_sex) {
      sex <- ifelse({{ sex }} == "m", 1, 2)
    } else {
      {{ sex }}
    }
  )

  df <- df |>
    mutate(
      sex = !!recode_sex
    ) |>
    addWGSR(
      sex = {{ "sex" }},
      firstPart = {{ "weight" }},
      secondPart = {{ "height" }},
      index = "wfh",
      digits = 3
    ) |>
    mutate(
      flag_wfhz = do.call(flag_outliers, list(.data$wfhz, type = "zscore"))
    )
  tibble::as_tibble(df)
}



#'
#' @rdname wrangler
#'
#' @export
#'
process_muac_data <- function(df,
                              sex,
                              muac,
                              age = NULL,
                              .recode_sex = TRUE,
                              .recode_muac = TRUE,
                              unit = c("cm", "mm", "none")) {
  unit <- match.arg(unit)

  recode_sex <- quote(
    if (.recode_sex) {
      sex <- ifelse({{ sex }} == "m", 1, 2)
    } else {
      {{ sex }}
    }
  )

  rec_muac <- quote(
    if (.recode_muac && unit == "cm") {
      muac <- recode_muac({{ muac }}, unit = "cm")
    } else if (.recode_muac && unit == "mm") {
      muac <- recode_muac({{ muac }}, unit = "mm")
    } else {
      {{ muac }}
    }
  )

  if (!is.null({{ age }})) {
    df <- df |>
      mutate(
        muac = !!rec_muac,
        sex = !!recode_sex,
      ) |>
      addWGSR(
        sex = "sex",
        firstPart = "muac",
        secondPart = "age_days",
        index = "mfa",
        digits = 3
      )|>
      mutate(
        flag_mfaz = do.call(flag_outliers, list(.data$mfaz, type = "zscore"))
      )
  } else {
    df <- df |>
      mutate(
        sex = !!recode_sex,
        flag_muac = do.call(flag_outliers, list({{ muac }}, type = "crude"))
      )
  }
  tibble::as_tibble(df)
}
