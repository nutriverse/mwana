#'
#' Calculate child's age in months
#'
#' @description
#' Calculate child's age in months based on the date of birth and the date of
#' data collection.
#'
#' @param dos A `Date` vector of date of data collection.
#'
#' @param dob A `Date` vector of the child's date of birth.
#'
#' @returns A `numeric` vector of child's age in months. Any value less
#' than 6.0 and greater than or equal to 60.0 months are set to NA.
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
  ## Enforce the class of `dos` ----
  if (!is(dos, "Date")) {
    stop(
      "`dos` must be a vector of class Date not ", class(dos), 
      ". Please try again."
    )
  }

  ## Enforce the class of `dob` ----
  if (!is(dob, "Date")) {
    stop(
      "`dob` must be a vector of class Date not ", 
      class(dob), ". Please try again."
    )
  }

  ## Calculate age in months ----
  int <- dos - dob
  age_mo <- int / (365.25 / 12)
  age_mo <- ifelse(age_mo < 6.0 | age_mo >= 60.0, NA, age_mo)
  age_mo
}


#'
#' Identify, flag, and remove outliers
#'
#' @description
#' Identify outlier z-scores for weight-for-height (WFHZ) and MUAC-for-age 
#' (MFAZ) following the SMART methodology. The function can also be used to 
#' detect outliers for height-for-age (HFAZ) and weight-for-age (WFAZ) z-scores
#' following the same approach.
#'
#' For flagging z-scores, z-scores that deviate substantially from the sample's 
#' z-score mean are considered outliers and are unlikely to reflect accurate 
#' measurements. For raw MUAC, values that are less than 100 millimeters or 
#' greater than 200 millimeters are considered outliers as recommended by
#' Bilukha & Kianian (2023). Including these values in the analysis could 
#' compromise the accuracy of the resulting estimates.
#' 
#' To remove outliers, their values are set to NA rather than removing the
#' record from the dataset. This process is also called *censoring*. By
#' assigning NA values to these outliers, they can be effectively removed
#' during statistical operations with functions that allow for removal of NA 
#' values such as [mean()] for getting the mean value or [sd()] for getting the
#' standard deviation.
#'
#' @param x A `numeric` vector of WFHZ, MFAZ, HFAZ, WFAZ or raw MUAC values.
#' Raw MUAC values should be in millimetre units.
#'
#' @param .from Either "zscores" or "raw_muac" for type of data to flag 
#' outliers from.
#'
#' @return An vector of the same length as `x` of flagged records coded as
#' `1` for a flagged record and `0` for a non-flagged record.
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
#' ## Sample data of raw MUAC values ----
#' x <- anthro.01$muac
#'
#' ## Apply the function with `.from` set to "raw_muac" ----
#' m <- flag_outliers(x, .from = "raw_muac")
#' head(m)
#'
#' ## Sample data of z-scores (be it WFHZ, MFAZ, HFAZ or WFAZ) ----
#' x <- anthro.02$mfaz
#'
#' # Apply the function with `.from` set to "zscores" ----
#' z <- flag_outliers(x, .from = "zscores")
#' tail(z)
#'
#' @rdname outliers
#' @export
#'
flag_outliers <- function(x, .from = c("zscores", "raw_muac")) {
  ## Enforce the options in `.from` ----
  .from <- match.arg(.from)

  ## Enforce the class of `x` ----
  if (!is.numeric(x)) {
    stop(
      "`x` must be of class numeric not ", 
      class(x), ". Please try again."
    )
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
#' Remove outliers
#'
#' @examples
#' ## With `.from` set to "zscores" ----
#' z <- remove_flags(
#'   x = wfhz.01$wfhz,
#'   .from = "zscores"
#' )
#'
#' head(z)
#'
#' ## With `.from` set to "raw_muac" ----
#' m <- remove_flags(
#'   x = mfaz.01$muac,
#'   .from = "raw_muac"
#' )
#'
#' tail(m)
#'
#' @rdname outliers
#' @export
#'
remove_flags <- function(x, .from = c("zscores", "raw_muac")) {
  ## Enforce options in `.from` ----
  .from <- match.arg(.from)

  ## Enforce the class of `x` ----
  if (!is.numeric(x)) {
    stop(
      "`x` must be of class numeric not ", 
      class(x), ". Please try again."
    )
  }

  ## Control flow based on `.from` ----
  switch(.from,
    ### Remove flags when `.from` = "zscores" ----
    "zscores" = {
      mean_x <- mean(x, na.rm = TRUE)
      zs <- ifelse(
        (x < (mean_x - 3) | x > (mean_x + 3)) | is.na(x), NA_real_, x
      )

      zs
    },
    ### Remove flags when `.from` = "raw_muac" ----
    "raw_muac" = {
      cr <- ifelse(x < 100 | x > 200 | is.na(x), NA_integer_, x)
      
      cr
    }
  )
}


#'
#' Convert MUAC values to either centimeters or millimeters
#'
#' @param x A vector of raw MUAC values. The class can either be `double` or 
#' `numeric` or `integer`.
#'
#' @param .to Either "cm" (centimeters) or "mm" (millimeters) for the unit of 
#' measurement to convert MUAC values to.
#'
#' @returns A `numeric` vector of the same length as `x` with values set to
#' specified unit of measurement.
#'
#' @examples
#' ## Recode from millimeters to centimeters ----
#' muac_cm <- recode_muac(
#'   x = anthro.01$muac,
#'   .to = "cm"
#' )
#' head(muac_cm)
#'
#' ## Using the `muac_cm` object to recode it back to "mm" ----
#' muac_mm <- recode_muac(
#'   x = muac_cm,
#'   .to = "mm"
#' )
#' tail(muac_mm)
#'
#' @export
#'
recode_muac <- function(x, .to = c("cm", "mm")) {
  ## Enfornce the options in `.to` ----
  .to <- match.arg(.to)

  ## Enforce the class of `x` ----
  if (!(is.numeric(x) | is.double(x) | is.integer(x))) {
    stop(
      "`x` must be of class numeric or integer or double not ", 
      class(x), ". Please try again."
    )
  }

  ## Recode muac conditionally ----
  switch(.to,
    ### Recode to centimeters ----
    "cm" = {
      #### Enforce measuring unit is in "mm" ----
      if (any(grepl("\\.", x))) {
        stop("MUAC values are not in millimeters. Please try again.")
      }
      #### Convert MUAC to cm ----
      z <- (x / 10)
      z
    },

    ### Recode to millimeters ----
    "mm" = {
      #### Enforce measuring unit is in "cm" ----
      if (all(!grepl("\\.", x))) {
        stop("MUAC values are not in centimeter. Please try again.")
      }
      #### Convert MUAC to mm ----
      z <- (x * 10)
      z
    }
  )
}
