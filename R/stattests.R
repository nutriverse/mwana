#'
#' Test for statistical difference between the proportion of children aged 24 to
#'  59 months old over those aged 6 to 23 months old
#'
#' @description
#' Calculate the observed age ratio of children aged 24 to 59 months old over
#' those aged 6 to 23 months old and test if there is a statistical difference
#' between the observed and the expected.
#'
#' @param age A vector of class `numeric` of child's age in months. If different
#' than expected, the function will stop execution and return an error message
#' indicating the type of mismatch.
#'
#' @param .expectedP The expected proportion of children aged 24 to 59 months
#' old over those aged 6 to 23 months old. This is estimated to be 0.66.
#'
#' @returns A vector of class `list` of three statistics: `p` for p-value of the
#' statistical difference between the observed and the expected proportion of
#' children aged 24 to 59 months old over those aged 6 to 23 months old;
#'  `observedR` and `observedP` for the observed ratio and proportion respectively.
#'
#' @details
#' This function should be used specifically when assessing the quality of MUAC data.
#' For age ratio test of children aged 6 to 29 months old over 30 to 59 months old, as
#' performed in the SMART plausibility check, use [nipnTK::ageRatioTest()] instead.
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
  if(!is.numeric(age)) {
    stop("`x` must be of class 'numeric'; not ", shQuote(class(x)), ". Please try again.")
  }

  ## Calculate observed proportion and ratio ----
  x <- ifelse(age >= 24, 1, 2)
  sum_o24 <- sum(na.omit(x == 1))
  sum_u24 <- sum(na.omit(x == 2))
  total <- sum(table(na.omit(x)))
  ratio <- sum_o24 / sum_u24
  prop <- sum_o24 / total

  ## Stats test with Yates continuity correction set to false ----
  test <- prop.test(sum_o24, total, p = .expectedP, correct = FALSE)

  ## Return ----
  list(
    p = test$p.value,
    observedR = ratio,
    observedP = prop
  )
}
