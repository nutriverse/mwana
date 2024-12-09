#'
#' Wrangle child's age
#'
#' @description
#' Wrangle child's age for downstream analysis. This includes calculating age
#' in months based on the date of data collection and the child's date of birth, 
#' and setting to NA the age values that are less than 6.0 and greater than or 
#' equal to 60.0 months old.
#'
#' @param df A `data.frame` object to wrangle age from.
#'
#' @param dos A `Date` vector of dates when data collection was conducted. 
#' Default is NULL.
#'
#' @param dob A `Date` vector of dates of birth of child. Default is NULL.
#'
#' @param age A `numeric` vector of child's age in months. In most cases this 
#' will be estimated using local event calendars or calculated age in months
#' based on date of data collection and date of birth of child.
#'
#' @param .decimals The number of decimal places to round off age to. Default is 
#' 2.
#'
#' @returns A `tibble` based on `df`. The variable `age` will be automatically
#' filled in each row where age value was missing and both the child's
#' date of birth and the date of data collection are available. Rows where `age`
#' is less than 6.0 and greater than or equal to 60.0 months old will be set to 
#' NA. Additionally, a new variable named `age_days` of class `double` for
#' calculated age of child in days is added to `df`.
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
#' mw_wrangle_age(
#'   df = df,
#'   dos = surv_date,
#'   dob = birth_date,
#'   age = age,
#'   .decimals = 3
#' )
#'
#' @export
#'
mw_wrangle_age <- function(df,
                           dos = NULL,
                           dob = NULL,
                           age,
                           .decimals = 2) {
  ## Difuse and evaluate arguments ----
  dos <- rlang::eval_tidy(enquo(dos), df)
  dob <- rlang::eval_tidy(enquo(dob), df)
  age <- rlang::eval_tidy(enquo(age), df)

  ## Calculate child's age in months then in days ----
  if (!is.null(dob) | !is.null(dos)) {
    ## Check if the class of vector "age" is "numeric" ----
    if (!is.numeric(age)) {
      stop(
        "`age` must be of class numeric not ", 
        class(age), ". Please try again."
      )
    }

    ## Calculate age in months ----
    df <- dplyr::mutate(
      .data = df,
      age = ifelse(is.na(!!age), get_age_months(dob = dob, dos = dos), age),
      age_days = round(.data$age * (365.25 / 12), .decimals)
    )
  } else {
    ## Enforce the class of `age` ----
    if (!is.numeric(age)) {
      stop(
        "`age` must be of class numeric not ", 
        class(age), ". Please try again."
      )
    }

    ## Calculate age in months ----
    df <- dplyr::mutate(
      .data = df,
      age_days = round(age * (365.25 / 12), .decimals)
    )
  }

  ## Return df ----
  tibble::as_tibble(df)
}
