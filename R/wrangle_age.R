#'
#' Calculate child's age in days
#'
#' @param x A double vector of child's age in months.
#'
#' @returns A double vector of the same length as `x` of age in days.
#'
#' @keywords internal
#'
convert_month_to_days <- function(x) {
  x * (365.25 / 12)
}




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
#' @returns A vector of class `double` for child's age in months with two decimal places.
#' Any value less than 6.0 and greater than or equal to 60.0 months will be set to `NA`.
#'
#' @keywords internal
calculate_age_in_months <- function (dos, dob) {
  avg_day <- 365.25 / 12
  int <- dos - dob
  age_mo <- round(int / avg_day, digits = 2)
  age_mo <- ifelse(age_mo < 6.0 | age_mo >= 60.0, NA, age_mo)
}




#'
#' Wrangle child's age
#'
#' @description
#' Wrangle child's age for downstream analysis. This includes calculating age
#' in months based on the date of data collection and child's date of birth and
#' setting to `NA` the age values that are less than 6.0 and greater than or equal
#' to 60.0 months old.
#'
#' @param df A dataset of class `data.frame` to process age from.
#'
#' @param dos A vector of class `Date` for date of data collection.
#' Default is `NULL`.
#'
#' @param dob A vector of class `Date` for child's date of birth.
#' Default is `NULL`.
#'
#' @param age A vector of class `integer` of age in months, usually estimated
#' using local event calendars.
#'
#' @returns A `data.frame` based on `df`. The variable `age` that is required to be
#' included in `df` will be filled where applicable with the age in months for
#' each row of data in `df`. A new variable for `df` named `age_days` will be
#' created. Values for `age` and `age_days` for children less than 6.0 and greater
#' than or equal to 60.0 months old will be set to `NA`.
#'
#' @examples
#'
#' ## A sample data ----
#'
#' df <- data.frame(
#' surv_date = as.Date(c(
#' "2023-01-01", "2023-01-01", "2023-01-01", "2023-01-01", "2023-01-01")),
#' birth_date = as.Date(c(
#' "2019-01-01", NA, "2018-03-20", "2019-11-05", "2021-04-25")),
#' age = c(NA, 36, NA, NA, NA)
#' )
#'
#' ## Apply the function ----
#' df |>
#' mw_wrangle_age(
#' dos = surv_date,
#' dob = birth_date,
#' age = age
#' )
#'
#' @export
#'
mw_wrangle_age <- function(df, dos = NULL, dob = NULL, age) {

  ## Diffuse arguments ----
  dos <- substitute(dos)
  dob <- substitute(dob)
  age <- substitute(age)

  ## Calculate chid's age in months then in days ----
  if (!is.null(dob) || !is.null(dos)) {
    df <- df |>
      mutate(
        age = ifelse(
          is.na(eval(age)),
          calculate_age_in_months(
            dob = eval(dob), dos = eval(dos)), eval(age)),
        age_days = round(age * 30.44, 2)
        )
  } else {
    df <- df |>
      mutate(
        age_days = round(eval(age) * 30.44, 2)
      )
  }

  ## Return df ----
  tibble::as_tibble(df)
}
