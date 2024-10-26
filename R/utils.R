#'
#' Calculate child's age in months
#'
#' @description
#' Calculate child's age in months based on date of birth and the data collection date.
#'
#' @param dos A vector of class `Date` for data collection date.
#'
#' @param dob A vector of class `Date` for child's date of birth.
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
  ## Check if vector dos is 'Date' ----
  if (!is(dos, "Date")) {
    stop("Child's birthdate should be of class 'Date'. Please try again.")
  }

  ## Check if vector dob is 'Date' ----
  if (!is(dob, "Date")) {
    stop("Date of data collection should be of class 'Date'. Please try again.")
  }

  int <- dos - dob
  age_mo <- int / (365.25 / 12)
  age_mo <- ifelse(age_mo < 6.0 | age_mo >= 60.0, NA, age_mo)
  age_mo
}

