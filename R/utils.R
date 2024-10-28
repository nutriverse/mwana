#'
#' Calculate child's age in months
#'
#' @description
#' Calculate child's age in months based on the date of birth and the date of
#' data collection.
#'
#' @param dos A vector of class `Date` for the date of data collection. If the class
#' is different than expected, the function will stop execution and return an error
#' message indicating the type of mismatch.
#'
#' @param dob A vector of class `Date` for the child's date of birth. If the class
#' is different than expected, the function will stop execution and return an error
#' message indicating the type of mismatch.
#'
#' @returns A vector of class `numeric` for child's age in months. Any value less
#' than 6.0 and greater than or equal to 60.0 months will be set to `NA`.
#'
#' @examples
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
#'
get_age_months <- function(dos, dob) {
  ## Check if the class of vector "dos" is "Date" ----
  if (!is(dos, "Date")) {
    stop("`dos` must be a vector of class 'Date'; not ", shQuote(class(dos)), ". Please try again.")
  }

  ## Check if the class of vector "dob" is "Date" ----
  if (!is(dob, "Date")) {
    stop("`dob` must be a vector of class 'Date'; not ", shQuote(class(dob)), ". Please try again.")
  }

  ## Calculate age in months ----
  int <- dos - dob
  age_mo <- int / (365.25 / 12)
  age_mo <- ifelse(age_mo < 6.0 | age_mo >= 60.0, NA, age_mo)
  age_mo
}


#'
#'
#' Identify, flag outliers and remove them
#'
#' @description
#' Identify outlier z-scores for weight-for-height (WFHZ) and MUAC-for-age (MFAZ)
#' following the SMART methodology. The function can be used, as well, to detect
#' outliers for height-for-age (HFAZ) and weight-for-age (WFAZ) z-scores
#' following the same approach.
#'
#' For raw MUAC values, outliers are identified when a value is less than 100
#' millimeters or greater than 200 millimeters.
#'
#' Removing outliers consists in setting the outlier record to `NA` and not necessarily
#' to delete it from the dataset. This is useful in the analysis procedures
#' where outliers must be removed, such as the analysis of the standard deviation.
#'
#' @param x A vector of class `numeric` of WFHZ, MFAZ, HFAZ, WFAZ or raw MUAC values.
#' The latter should be in millimeters. If the class is different than expected,
#' the function will stop execution and return an error message indicating the
#' type of mismatch.
#'
#' @param .from A choice between `zscores` and `raw_muac` for where outliers should be
#' detected and flagged from.
#'
#' @return A vector of the same length as `x` for flagged records coded as
#' `1` for is a flag and `0` not a flag.
#'
#' @details
#' For z-score-based detection, flagged records represent outliers that deviate
#' substantially from the sample's z-score mean, making them unlikely to reflect
#' accurate measurements. For raw MUAC values, flagged records are those that fall
#' outside the acceptable fixed range. Including such outliers in the analysis could
#' compromise the accuracy and precision of the resulting estimates.
#'
#' The flagging criterion used for raw MUAC values is based on a recommendation
#' by Bilukha, O., & Kianian, B. (2023).
#'
#' @references
#' Bilukha, O., & Kianian, B. (2023). Considerations for assessment of measurement
#' quality of mid‐upper arm circumference data in anthropometric surveys and
#' mass nutritional screenings conducted in humanitarian and refugee settings.
#' *Maternal & Child Nutrition*, 19, e13478. Available at <https://doi.org/10.1111/mcn.13478>
#'
#' SMART Initiative (2017). *Standardized Monitoring and Assessment for Relief
#' and Transition*. Manual 2.0. Available at: <https://smartmethodology.org>.
#'
#'
#' @examples
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
  ## Ensure that only predefined options are supplied ----
  .from <- match.arg(.from)

  ## Check if the class of vector "x" is "numeric" ----
  if (!is.numeric(x)) {
    stop("`x` must be of class numeric; not a ", shQuote(class(x)), ". Please try again.")
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
#' ## With .from set to "raw_muac" ----
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
    stop("`x` must be of class numeric; not a ", shQuote(class(x)), ". Please try again.")
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
#' Before to covert, the function checks if the supplied MUAC
#' values are in the opposite unit of the intended conversion. If not,
#' execution stops and an error message is returned.
#'
#' @param x A vector of the raw MUAC values. The class can either be
#' `double` or `numeric` or `integer`. If different than expected, the function
#' will stop execution and return an error message indicating the type of mismatch.
#'
#' @param .to The unit to convert MUAC values to. Defaults to `cm`. If set to `cm`,
#' the function checks if the values are in `mm`; if not, it throws an error.
#' Similarly, if set to `mm`, the function ensures the values are in `cm` before
#' to proceed.
#'
#' @returns A numeric vector of the same length as `x`, with values converted
#' to the chosen unit.
#'
#' @examples
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
  if (!(is.numeric(x) | is.double(x) | is.integer(x))) {
    stop(
      "`x` must be of class 'numeric' or `integer` or 'double'; not ", shQuote(class(x)), ". Please try again."
    )
  }

  ## Recode muac conditionally ----
  switch(.to,
    ### Recode to centimeters ----
    "cm" = {
      #### Ensure that vector supplied is in "mm" ----
      if(any(grepl("\\.", as.character(x)))) {
        stop("MUAC values are not in millimeters. Please try again.")
      }
      #### Convert MUAC to cm ----
      z <- (x / 10)
      z
    },

    ### Recode to millimeters ----
    "mm" = {
      #### Ensure that vector supplied is in  "cm" ----
      if(all(!grepl("\\.", as.character(x)))) {
        stop("MUAC values are not in centimeter. Please try again.")
      }
      #### Convert MUAC to mm ----
      z <- (x * 10)
      z
    }
  )
}
