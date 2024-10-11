#'
#' Transform age in months to days
#'
#' @param x A numeric vector containing age values in months.
#'
#' @returns A numeric vector, of the same length as the input variable, containing
#' age values in days.
#'
#'
compute_month_to_days <- function(x) {
  x * (365.25 / 12)
}

#'
#' Calculate age in months
#'
#' @description
#' `compute_age_in_months()` calculates age in months from on the basis of
#' difference between the data collection date and the child's date of birth.
#' It works inside [dplyr::mutate()] or [base::transform()].
#'
#' @param surv_date A vector of class "Date" holding values corresponding to
#' the date of data collection.
#'
#' @param birth_date A vector of class "Date" holding values corresponding to
#' the child's date of birth.
#'
#' @returns A numeric vector named `age` holding age values in months with two
#' decimal places. Any value outside the range of 6.0 to 59.99 is replaced with
#' `NA`.
#'
#'
compute_age_in_months <- function (surv_date, birth_date) {
  avg_day <- 365.25 / 12
  int <- surv_date - birth_date
  age_mo <- round(int / avg_day, digits = 2)
  age_mo <- ifelse(age_mo < 6.0 | age_mo >= 60.0, NA, age_mo)
}

#'
#' Process age
#'
#' @description
#' `process_age()` helps you to get the variable age in the format needed for
#' the analyses in the downstream workflow. Fundamentally, it calculates age in
#' months from on the basis of the difference between the data collection date
#' and the child's date of birth and then censors age values that are out of range.
#'
#' @param df Input data frame holding the required variables.
#'
#' @param svdate A vector of class "Date" holding values corresponding to
#' the data collection date. Default is `NULL`.
#'
#' @param birdate A vector of class "Date" holding values corresponding to
#' the child's date of birth. Default is `NULL`.
#'
#' @param age A numeric vector holding age values in months, usually estimated
#' using local event calendars.
#'
#' @returns A data frame of the same length as the input with an additional
#' column. A new variable, `age_day`, is added to the output data frame whilst
#' the `age` variable gets filled where applicable, and then any values outside
#' the range of 6.0 to 59.99 months get replaced with `NA`.
#'
#' @examples
#'
#' ## A sample data ----
#' df <- data.frame(
#' survy_date = as.Date(c(
#' "2023-01-01", "2023-01-01", "2023-01-01", "2023-01-01", "2023-01-01")),
#' birthdate = as.Date(c(
#' "2019-01-01", NA, "2018-03-20", "2019-11-05", "2021-04-25")),
#' age = c(NA, 36, NA, NA, NA)
#' )
#'
#' ## Apply the function ----
#' df |>
#' process_age(
#' svdate = "survy_date",
#' birdate = "birthdate",
#' age = age
#' )
#'
#' @export
#'

process_age <- function(df, svdate = NULL, birdate = NULL, age) {
  if (!is.null({{ birdate }}) || !is.null({{ svdate }})) {
    df <- df |>
      mutate(
        age = ifelse(
          is.na({{ age }}),
          compute_age_in_months(
            birth_date = !!sym({{ birdate }}), surv_date = !!sym({{ svdate }})
          ), {{ age }}),
        age_days = round(age * 30.44, 2)
      )

  } else {
    df <- df |>
      mutate(
        age_days = round({{ age }} * 30.44, 2)
      )
  }
  tibble::as_tibble(df)
}

#'
#' Test the proportion of children aged 24 to 59 months over 6 to 23 months old
#'
#' @description
#' Age ratio test of the proportion of children aged 24 to 59 months over those
#' aged 6 to 23 months old.
#'
#' @param age A numeric vector holding child's age in months.
#'
#' @param .expectedP The expected proportion of children aged 24 to 59 months
#' old over those aged 6 to 23 months old. As in the
#' [SMART MUAC tool](https://smartmethodology.org/survey-planning-tools/updated-muac-tool/),
#' this is estimated at 0.66.
#'
#' @returns A vector of class "list" holding three statistics: `p` for p-value,
#'  `observedR` for the observed ratio and `observedP` for the observed proportion
#'  of children aged 24 to 59 months over those aged 6 to 24 months old.
#'
#'  @details
#' `age_ratio_test()` should be used specifically for assessing MUAC data. For
#' age ratio tests of children ages 6 to 29 months and 30 to 59 months old, as
#' performed in the SMART plausibility checks, use [nipnTK::ageRatioTest()] instead.
#'
#' @examples
#'
#' ## A sample data ----
#' age <- seq(6,59) |>
#' sample(300, replace = TRUE)
#'
#' ## Apply the function ----
#' age_ratio_test(
#' age = age,
#' .expectedP = 0.66
#' )
#'
#' @export
#'
age_ratio_test <- function(age, .expectedP = 0.66) {

  x <- ifelse(age >= 24, 1, 2)
  sum_o24 <- sum(na.omit(x == 1))
  sum_u24 <- sum(na.omit(x == 2))
  total <- sum(table(na.omit(x)))
  ratio <- sum_o24 / sum_u24
  prop <- sum_o24 / total
  test <- prop.test(sum_o24, total, p = .expectedP, correct = FALSE)

  list(
    p = test$p.value,
    observedR = ratio,
    observedP = prop
  )
}
