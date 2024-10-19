#'
#' Calculate child's age in days
#'
#' @param x A double vector of child's age in months.
#'
#' @returns A double vector of the same length as `x` of age in days.
#'
#'
compute_month_to_days <- function(x) {
  x * (365.25 / 12)
}




#'
#' Calculate child's age in months
#'
#' @description
#' Calculate child's age in months based on date of birth and the data collection date.
#'
#' @param surv_date A vector of class `Date` for data collection date.
#'
#' @param birth_date A vector of class `Date` for child's date of birth.
#'
#' @returns A vector of class `double` for child's age in months with two decimal places.
#' Any value less than 6.0 and greater than or equal to 60.0 months will be set to `NA`.
#'
#'
compute_age_in_months <- function (surv_date, birth_date) {
  avg_day <- 365.25 / 12
  int <- surv_date - birth_date
  age_mo <- round(int / avg_day, digits = 2)
  age_mo <- ifelse(age_mo < 6.0 | age_mo >= 60.0, NA, age_mo)
}




#'
#' Process child's age
#'
#' @description
#' Process child's age for downstream analysis. This includes calculating age
#' in months based on the date of data collection and child's date of birth and
#' setting to `NA` the age values that are less than 6.0 and greater than or equal
#' to 60.0 months old.
#'
#' @param df A dataset of class `data.frame` to process age from.
#'
#' @param svdate A vector of class `Date` for date of data collection.
#' Default is `NULL`.
#'
#' @param birdate A vector of class `Date` for child's date of birth.
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
#' Test for statistical difference between the proportion of children aged 24 to
#'  59 months old over those aged 6 to 23 months old
#'
#' @description
#' Calculate the observed age ratio of children aged 24 to 59 months old over
#' those aged 6 to 23 months old and test if there is a statistical difference
#' between the observed and the expected.
#'
#' @param age A double vector of age in months.
#'
#' @param .expectedP The expected proportion of children aged 24 to 59 months
#' old over those aged 6 to 23 months old. This is estimated to be 0.66 as in the
#' [SMART MUAC tool](https://smartmethodology.org/survey-planning-tools/updated-muac-tool/).
#'
#' @returns A vector of class `list` of three statistics: `p` for p-value of the
#' statistical difference between the observed and the expected proportion of
#' children aged 24 to 59 months old over those aged 6 to 23 months old;
#'  `observedR` and `observedP` for the observed ratio and proportion respectively.
#'
#'  @details
#' This function should be used specifically for assessing MUAC data. For
#' age ratio tests of children aged 6 to 29 months old over 30 to 59 months old, as
#' performed in the SMART plausibility check, use [nipnTK::ageRatioTest()] instead.
#'
#' @examples
#'
#' ## An example of application using `anthro.02` dataset ----
#' age_ratio_test(
#' age = anthro.02$age,
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
