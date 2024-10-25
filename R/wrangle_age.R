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
  int <- dos - dob
  age_mo <- int / (365.25 / 12)
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
#' @param df A dataset of class `data.frame` to wrangle age from.
#'
#' @param dos A vector of class `Date` for date of data collection from the
#' `df`. Default is `NULL`.
#'
#' @param dob A vector of class `Date` for child's date of birth from the `df`.
#' Default is `NULL`.
#'
#' @param age A vector of class `numeric` of age in months, usually estimated
#' using local event calendars.
#'
#' @param .decimals The number of decimals places to which the age should be rounded.
#' Default is 2.
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
#' age = age,
#' .decimals = 3
#' )
#'
#' @export
#'
mw_wrangle_age <- function(df,
                           dos = NULL,
                           dob = NULL,
                           age,
                           .decimals = 2) {

  ## Diffuse arguments ----
  dos <- rlang::enquo(dos)
  dob <- rlang::enquo(dob)
  age <- rlang::enquo(age)

  ##Evaluate quosures to get actual columns ----
  x <- rlang::eval_tidy(dos, df)
  y <- rlang::eval_tidy(dob, df)
  z <- rlang::eval_tidy(age, df)

  ## Calculate chid's age in months then in days ----
  if (!rlang::quo_is_null(dob) | !rlang::quo_is_null(dos)) {

    ## Check if vector dos is.Date ----
    if(!lubridate::is.Date(x)) {
      stop("Child's birthdate should be of class 'Date'. Please try again.")
    }

    ## Check if vector dos is.Date ----
    if(!lubridate::is.Date(y)) {
      stop("Date of data collection should be of class 'Date'. Please try again.")
    }

    ## Check if vector dos is.Date ----
    if(!is.numeric(z)) {
      stop("Child's age should be of class 'integer'. Please try again.")
    }

    ## Calculate age in months ----
    df <- df |>
      mutate(
        age = ifelse(
          is.na(!!age),
          calculate_age_in_months(dob = !!dob, dos = !!dos), !!age),
        age_days = round(.data$age * (365.25 / 12), .decimals)
        )
  } else {

    ## Check if vector age is.numeric ----
    if(!is.numeric(z)) {
      stop("Child's age should be of class 'integer'. Please try again.")
    }

    ## Calculate age in months ----
    df <- df |>
      mutate(
        age_days = round(!!age * (365.25 / 12), .decimals)
      )
  }

  ## Return df ----
  tibble::as_tibble(df)
}
