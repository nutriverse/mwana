#'
#'
#' Identify and flag outliers
#'
#' @description
#' Outliers are extreme values that deviate remarkably from the mean, making
#' them unlikely to be accurate measurements. `flag_outliers()` helps you to
#' identify them whether in the WFHZ, the MFAZ or the absolute MUAC values.
#'
#' @param x A numeric vector holding either the WFHZ, the MFAZ values, or the
#' absolute MUAC values (in millimeters).
#'
#' @param type The method you wish `flag_outliers()` to identify flag outliers
#' in the data. A choice between "zscore" (for WFHZ and MFAZ), and "crude" (for
#' absolute MUAC values).
#'
#' @param unit A choice between "zscore" (for WFHZ and MFAZ), and "crude" (for
#' absolute MUAC values).
#'
#' @return A vector of the same length as input holding dummy values: 1 for is
#' a flag and 0 is not a flag.
#'
#' @details
#' The flagging criteria for the WFHZ is as in
#' [SMART plausibility check](https://smartmethodology.org/). As for the MFAZ, it
#' uses the same criteria as WFHZ, whilst a fixed flagging criteria is used for
#' absolute MUAC values. This is as recommended by
#' [Bilukha, O., & Kianian, B. (2023).](https://doi.org/10.1111/mcn.13478)
#'
#'
#' @examples
#'
#' ## Sample data for absolute MUAC values ----
#' x <- c(90, 110, 140, 200, 119, 235)
#'
#' ## Apply `flag_outliers()` with type set to "crude" ----
#' flag_outliers(x, type = "crude")
#'
#' ## Sample data for MFAZ or for WFHZ values ----
#' x <- c(-2.265, -5.275, -0.72, -2.261, -2.264, -4.451, -2.261, -1.828)
#'
#' # Apply `flag_outliers()` with type set to "zscore" ----
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
#' Recode the MUAC values into either centimeters or millimeters as required.
#' `recode_muac()` works inside [dplyr::mutate()] or [base::transform()].
#'
#' @param muac A numeric vector holding the absolute MUAC values.
#'
#' @param unit A choice of the unit to which you wish to convert the MUAC
#' values into.
#'
#' @returns A numeric vector of the same length as input, with values converted
#' into your chosen unit.
#'
#' @examples
#'
#' ## A sample of MUAC data in millimeters ----
#' muac <- seq(90, 250, by = 4)
#'
#' ## Apply the function ----
#' recode_muac(muac, unit = "cm")
#'
#' ## A sample of MUAC data in centimeters ----
#' muac <- seq(9.0, 25.0, by = 0.2)
#'
#' # Apply the function ----
#' recode_muac(muac, unit = "mm")
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
#' Process and censor weight-for-height and MUAC data
#'
#' @description
#' This is the job of `process_wfhz_data` and `process_muac_data()`. They are
#' responsible for computing the weight-for-height and the muac-for-age z-scores
#' respectively, and censor the data by flagging outliers based on the SMART flags.
#' For the latter, if age is not supplied, the function censors the absolute MUAC
#' values.
#'
#' @param df The input data frame with the required variables.
#'
#' @param sex A numeric or character vector of child's sex. Code values should
#' either be 1 or "m" for boy and 2 or "f" for girl. The variable name must be
#' sex, otherwise it will not work.
#'
#' @param .recode_sex Logical. It asks whether sex should be recoded. In the end,
#' the variable sex have values coded as 1 for boy and 2 for girl. Setting
#' `.recode_sex = TRUE` works over "m" and "f" values. If your vector is coded
#' differently, make sure to put it in "m" and "f" or in 1 or 2 right away.
#'
#' @param muac A numeric vector holding the absolute MUAC values.
#'
#' @param .recode_muac Logical. Choose between `TRUE` if you wish to recode
#' the MUAC values into either centimeters or millimeters.
#'
#' @param unit A choice of the unit to which you wish to convert the MUAC
#' variable into. Choose "cm" for centimeters, "mm" for millimeters and "none"
#' to leave as it is.
#'
#' @param age A numeric vector of child's age in months. It must be named age,
#' otherwise it will not work. For instance, if given as following: age = months
#' it will not work.
#'
#' @param weight A numeric vector holding the weight values of the child in
#' kilograms.
#'
#' @param height A numeric vector holding the height values of the child in
#' centimeters.
#'
#' @returns A data frame of the same length as the input with additional
#' columns: one named `wfhz` or `mfaz` that holds the zscore values, and the other
#' holding dummy values: 1 (is a flag) and 0 (is not a flag). For the
#' `process_muac_data` function, when age is not supplied, only `flag_muac` is
#' added. This refers to flags based based on absolute MUAC values as recommended by
#' [Bilukha, O., & Kianian, B. (2023).](https://doi.org/10.1111/mcn.13478).
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
#'  birthdate = as.Date(c(
#'  "2019-01-01", NA, "2018-03-20", "2019-11-05", "2021-04-25")),
#'  age = c(NA, 36, NA, NA, NA),
#'  sex = c("m", "f", "m", "m", "f"),
#'  muac = c(110, 130, 300, 123, 125)
#'  )
#'
#'  ### The application of the function ----
#'  df |>
#'  process_age(
#'  svdate = "survey_date",
#'  birdate = "birthdate",
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
process_muac_data <- function(df,
                              sex, muac, age = NULL,
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


#'
#' @rdname wrangler
#'
#' @export
#'
process_wfhz_data <- function(df, sex, weight, height, .recode_sex = TRUE) {

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
