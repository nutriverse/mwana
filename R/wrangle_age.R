#'
#'
#' Wrangle child's age
#'
#' @description
#' Wrangle child's age for downstream analysis. This includes calculating age
#' in months based on the date of data collection and the child's date of birth and
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
#' @param age A vector of class `numeric` of child's age in months. In most
#' cases this will be estimated using local event calendars; in some other
#' cases it can be a mix of the former and the one based on the child's
#' date of birth and the date of data collection.
#'
#' @param .decimals The number of decimals places to which the age should be rounded.
#' Default is 2.
#'
#' @returns A `data.frame` based on `df`. The variable `age` will be automatically
#' filled in each row where age value was missing and both the child's
#' date of birth and the date of data collection are available. Rows where `age`
#' is less than 6.0 and greater than or equal to 60.0 months old will be set to `NA`.
#' Additionally, a new variable for `df` named `age_days`, of class `double`, will
#' be created.
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

  ## Difuse and lazy evaluate arguments ----
  dos <- rlang::eval_tidy(rlang::enquo(dos), df)
  dob <- rlang::eval_tidy(rlang::enquo(dob), df)
  age <- rlang::eval_tidy(rlang::enquo(age), df)


  ## Calculate child's age in months then in days ----
  if (!is.null(dob) | !is.null(dos)) {

    ## Check if the class of vector "age" is "numeric" ----
    if (!is.numeric(age)) {
      stop("Child's age should be of class 'numeric'. Please try again.")
    }

    ## Calculate age in months ----
    df <- df |>
      mutate(
        age = ifelse(
          is.na(!!age),
          get_age_months(dob = dob, dos = dos), age
        ),
        age_days = round(.data$age * (365.25 / 12), .decimals)
      )
  } else {
    ## Check if the class of vector "z" is "numeric" ----
    if (!is.numeric(age)) {
      stop("Child's age should be of class 'numeric'. Please try again.")
    }

    ## Calculate age in months ----
    df <- df |>
      mutate(
        age_days = round(age * (365.25 / 12), .decimals)
      )
  }

  ## Return df ----
  tibble::as_tibble(df)
}
