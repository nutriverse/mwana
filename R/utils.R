#'
#' Calculate child's age in months
#'
#' @description
#' Calculate child's age in months based on the date of birth and the date of
#' data collection.
#'
#' @param dos A vector of class `Date` for the date of data collection.
#'
#' @param dob A vector of class `Date` for the child's date of birth.
#'
#' @returns A vector of class `numeric` for child's age in months. Any value less
#' than 6.0 and greater than or equal to 60.0 months will be set to `NA`.
#'
#' @examples
#'
#' ## Take two vectors of class "Date" ----
#' surv_date <- as.Date(
#'   c(
#'     "2024-01-05", "2024-01-05", "2024-01-05", "2024-01-08", "2024-01-08",
#'     "2024-01-08", "2024-01-10", "2024-01-10", "2024-01-10", "2024-01-11"
#'   )
#' )
#' bir_date <- as.Date(
#'   c(
#'     "2022-04-04", "2021-05-01", "2023-05-24", "2017-12-12", NA,
#'     "2020-12-12", "2022-04-04", "2021-05-01", "2023-05-24", "2020-12-12"
#'   )
#' )
#'
#' ## Apply the function ----
#' get_age_months(
#'   dos = surv_date,
#'   dob = bir_date
#' )
#'
#' @export
get_age_months <- function(dos, dob) {
  ## Check if the class of vector "dos" is "Date" ----
  if (!is(dos, "Date")) {
    stop("Child's date of birth should be of class 'Date'. Please try again.")
  }

  ## Check if the class of vector "dob" is "Date" ----
  if (!is(dob, "Date")) {
    stop("The date of data collection should be of class 'Date'. Please try again.")
  }

  ## Calculate age in months ----
  int <- dos - dob
  age_mo <- int / (365.25 / 12)
  age_mo <- ifelse(age_mo < 6.0 | age_mo >= 60.0, NA, age_mo)
  age_mo
}


#'
#'
#' Identify and flag outliers
#'
#' @description
#' Outliers are extreme values that deviate remarkably from the survey mean, making
#' them unlikely to be accurate measurements. This function identifies and signals
#' them based on a criterion set for the WFHZ, the MFAZ and for the absolute MUAC
#' values. For the raw MUAC values, outliers are identified and flagged using
#' a different approach as described below.
#'
#' @param x A vector of class `numeric` of WFHZ or MFAZ or raw MUAC values.
#' The latter should be in millimeters.
#'
#' @param .from A choice between `zscores` and `raw_muac` for where outliers should be
#' detected and flagged from.
#'
#' @return A vector of the same length as `x` of flagged observations that are
#' outliers: 1 for is a flag and 0 is not a flag.
#'
#' @details
#' The flagging criterion used for the WFHZ and the MFAZ is as in
#' [SMART plausibility check](https://smartmethodology.org/). A fixed flagging
#' criterion is used for the raw MUAC values. This is as recommended by
#' [Bilukha, O., & Kianian, B. (2023).](https://doi.org/10.1111/mcn.13478)
#'
#'
#' @examples
#'
#' ## Sample data for raw MUAC values ----
#' x <- anthro.01$muac
#'
#' ## Apply the function with type set to "raw_muac" ----
#' flag_outliers(x, .from = "raw_muac")
#'
#' ## Sample data for MFAZ or for WFHZ values ----
#' x <- anthro.02$mfaz
#'
#' # Apply the function with type set to "zscores" ----
#' flag_outliers(x, .from = "zscores")
#'
#' @rdname outliers
#' @export
#'
flag_outliers <- function(x, .from = c("zscores", "raw_muac")) {
  ## Ensure that only predefined options ----
  .from <- match.arg(.from)

  ## Check if the class of vector "x" is "numeric" ----
  if (!is.numeric(x)) {
    stop("Vector supplied should be of class 'numeric'. Please try again.")
  }

  ## Identify and flag outliers from zscores ----
  if (.from == "zscores") {
    mean_zscore <- mean(x, na.rm = TRUE)
    flags <- ifelse(x < (mean_zscore - 3) | x > (mean_zscore + 3), 1, 0)
    flags <- ifelse(is.na(x), NA, flags)
    flags

    ## Identify and flag outliers from raw MUAC values ----
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
#' @examples
#' ## With .from set to "zscores" ----
#' remove_flags(wfhz.01$wfhz, .from = "zscores")
#'
#' ## With .from set to "absolute" ----
#' remove_flags(mfaz.01$muac, .from = "raw_muac")
#'
#' @rdname outliers
#'
#' @export
#'
remove_flags <- function(x, .from = c("zscores", "raw_muac")) {
  ## Match arguments ----
  .from <- match.arg(.from)

  ## Check if the class of vector "x" is "numeric" ----
  if (!is.numeric(x)) {
    stop("Vector supplied should be of class 'numeric'. Please try again.")
  }

  ## Control flow based on .from ----
  switch(.from,
    ### Remove flags when .from = "zscores" ----
    "zscores" = {
      mean_x <- mean(x, na.rm = TRUE)
      zs <- ifelse((x < (mean_x - 3) | x > (mean_x + 3)) | is.na(x), NA_real_, x)
      zs
    },
    ### Remove flags when .from = "raw_muac" ----
    "raw_muac" = {
      cr <- ifelse(x < 100 | x > 200 | is.na(x), NA_integer_, x)
      cr
    }
  )
}


#'
#'
#'
#' Convert MUAC values to either centimeters or millimeters
#'
#' @description
#' Convert MUAC values to either centimeters or millimeters as required.
#'
#' @param x A vector of the raw MUAC values. The class can either be
#' `double` or `numeric`.
#'
#' @param .to A choice of the unit to which the MUAC values should be converted.
#'
#' @returns A numeric vector of the same length as `x`, with values converted
#' to the chosen unit.
#'
#' @examples
#'
#' ## Recode from millimeters to centimeters ----
#' muac <- anthro.01$muac
#' muac_cm <- recode_muac(muac, .to = "cm")
#'
#' ## Using the `muac_cm` object to recode it back to "mm" ----
#' muac_mm <- recode_muac(muac_cm, .to = "mm")
#'
#' @export
#'
recode_muac <- function(x, .to = c("cm", "mm")) {
  ## Check if unit's arguments match ----
  .to <- match.arg(.to)

  ## Check if the class of vector "x" is "numeric" or "double" ----
  if (!is.numeric(x) | !is.double(x)) {
    stop(
      "MUAC should be of class 'numeric' or 'double'. Please try again."
    )
  }

  ## Recode muac conditionally ----
  switch(.to,
    ### Recode to millimeters ----
    "mm" = {
      z <- (x * 10)
      z
    },
    ### Recode to centimeters ----
    "cm" = {
      z <- (x / 10)
      z
    }
  )
}
