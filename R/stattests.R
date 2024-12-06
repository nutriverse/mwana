#'
#' Test for statistical difference between the proportion of children aged 24 to
#' 59 months old over those aged 6 to 23 months old
#'
#' @description
#' Calculate the observed age ratio of children aged 24 to 59 months old over
#' those aged 6 to 23 months old and test if there is a statistically 
#' significant difference between the observed and the expected.
#'
#' @param age A `numeric` vector of child's age in months.
#'
#' @param .expectedP The expected proportion of children aged 24 to 59 months
#' old over those aged 6 to 23 months old. By default, this is expected to be 
#' 0.66.
#'
#' @returns A `list` object with three elements: `p` for p-value of the
#' difference between the observed and the expected proportion of children aged 
#' 24 to 59 months old over those aged 6 to 23 months old, `observedR` for the 
#' observed ratio, and `observedP` for the observed proportion.
#'
#' @details
#' This function should be used specifically when assessing the quality of MUAC 
#' data. For age ratio test of children aged 6 to 29 months old over 30 to 59 
#' months old, as performed in the SMART plausibility check, use 
#' [nipnTK::ageRatioTest()] instead.
#'
#' @references
#' SMART Initiative. *Updated MUAC data collection tool*. Available at:
#' <https://smartmethodology.org/survey-planning-tools/updated-muac-tool/>
#'
#' @examples
#' mw_stattest_ageratio(
#'   age = anthro.02$age,
#'   .expectedP = 0.66
#' )
#'
#' @export
#'
mw_stattest_ageratio <- function(age, .expectedP = 0.66) {
  ## Enforce the class of `age` ----
  if (!is.numeric(age)) {
    stop(
      "`age` must be of class numeric not ", class(age), 
      ". Please try again."
    )
  }

  ## Calculate observed proportion and ratio ----
  x <- ifelse(age >= 24, 1, 2)
  sum_o24 <- sum(na.omit(x == 1))
  sum_u24 <- sum(na.omit(x == 2))
  total <- sum(table(na.omit(x)))
  ratio <- sum_o24 / sum_u24
  prop <- sum_o24 / total

  ## Stats test with Yates continuity correction set to false ----
  test <- stats::prop.test(sum_o24, total, p = .expectedP, correct = FALSE)

  ## Return ----
  list(
    p = test$p.value,
    observedR = ratio,
    observedP = prop
  )
}
