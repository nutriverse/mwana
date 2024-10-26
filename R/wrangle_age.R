#'
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
#' @param age A vector of class `numeric` of child's age in months. In most of
#' the cases this will be estimated using local event calendars; in some other
#' cases it can be a mix of the former and age in months based on date of birth
#' and survey date.
#'
#' @param .decimals The number of decimals places to which the age should be rounded.
#' Default is 2.
#'
#' @returns A `data.frame` based on `df`. The variable `age` will be filled
#' automatically in each row where age values were missing and both the child's
#' date of birth and the date of data collection are available. Additionally,
#' a new variable for `df` named `age_days` will be created. Values for `age`
#' and `age_days` for children less than 6.0 and greater than or equal to 60.0
#' months old will be set to `NA`.
#'
#' @examples
#'
#' ## A sample data ----
#' df <- data.frame(
#'   surv_date = as.Date(c(
#'     "2023-01-01", "2023-01-01", "2023-01-01", "2023-01-01", "2023-01-01"
#'   )),
#'   birth_date = as.Date(c(
#'     "2019-01-01", NA, "2018-03-20", "2019-11-05", "2021-04-25"
#'   )),
#'   age = c(NA, 36, NA, NA, NA)
#' )
#'
#' ## Apply the function ----
#' df |>
#'   mw_wrangle_age(
#'     dos = surv_date,
#'     dob = birth_date,
#'     age = age,
#'     .decimals = 3
#'   )
#'
#' @export
#'
mw_wrangle_age <- function(df,
                           dos = NULL,
                           dob = NULL,
                           age,
                           .decimals = 2) {
  ## Difuse arguments ----
  dos <- rlang::enquo(dos)
  dob <- rlang::enquo(dob)
  age <- rlang::enquo(age)

  ## Evaluate quosures to get actual columns ----
  x <- rlang::eval_tidy(dos, df)
  y <- rlang::eval_tidy(dob, df)
  z <- rlang::eval_tidy(age, df)

  ## Calculate child's age in months then in days ----
  if (!rlang::quo_is_null(dob) | !rlang::quo_is_null(dos)) {
    ## Check if vector dos is.Date ----
    if (!is.numeric(z)) {
      stop("Child's age should be of class 'integer'. Please try again.")
    }

    ## Calculate age in months ----
    df <- df |>
      mutate(
        age = ifelse(
          is.na(!!age),
          get_age_months(dob = !!dob, dos = !!dos), !!age
        ),
        age_days = round(.data$age * (365.25 / 12), .decimals)
      )
  } else {
    ## Check if vector age is.numeric ----
    if (!is.numeric(z)) {
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
