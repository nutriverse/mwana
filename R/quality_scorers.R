#'
#' Score the acceptability classification of the standard deviation and percentage
#' of flagged data test results
#'
#' @description
#' Attribute a penalty point based on the acceptability classification in which
#' the plausibility test result falls.
#'
#' @param x A vector of class `character` of acceptability classification of the
#' plausibility test results.
#'
#' @returns A vector of class `integer` of the same length as `x` for the score.
#'
#' @details
#' The scoring criteria is as in [SMART Plausibility checks](https://smartmethodology.org/).
#'
#' @rdname scorer
#'
#' @keywords internal
#'
score_std_flags <- function(x) {
  ## Enforce the class of `x` ----
  if (!is.character(x)) {
    stop("`x` must be of class `character`; not ", shQuote(class(x)), ". Please try again.")
  }

  ## Score ----
  case_when(
    x == "Excellent" ~ 0,
    x == "Good" ~ 5,
    x == "Acceptable" ~ 10,
    x == "Problematic" ~ 20
  )
}


#'
#'
#' @rdname scorer
#'
#' @keywords internal
#'
score_agesexr_dps <- function(x) {
  ## Enforce the class of `x` ----
  if (!is.character(x)) {
    stop("`x` must be of class `character`; not ", shQuote(class(x)), ". Please try again.")
  }

  ## Score ----
  case_when(
    x == "Excellent" ~ 0,
    x == "Good" ~ 2,
    x == "Acceptable" ~ 4,
    x == "Problematic" ~ 10
  )
}

#'
#'
#' @rdname scorer
#'
#' @keywords internal
#'
score_skewkurt <- function(x) {
  ## Enforce the class of `x` ----
  if (!is.character(x)) {
    stop("`x` must be of class `character`; not ", shQuote(class(x)), ". Please try again.")
  }

  ## Score ----
  case_when(
    x == "Excellent" ~ 0,
    x == "Good" ~ 1,
    x == "Acceptable" ~ 3,
    x == "Problematic" ~ 5
  )
}

#'
#'
#' Get the overall acceptability score from the acceptability classification scores
#'
#' @description
#' Calculate the total amount of penalty points based on each plausibility test
#' result acceptability classification for WFHZ and MFAZ.
#'
#' @param df A dataset object of class `data.frame` to calculate from.
#'
#' @param .for A choice between "wfhz" and "mfaz" for the basis on which the
#' calculations should be made.
#'
#' @returns A `data.frame` based on `df` with a new column named `"quality_score"`
#' for the overall of acceptability (of quality) score.
#'
#' @keywords internal
#'
score_overall_quality <- function(cl_flags,
                                  cl_sex,
                                  cl_age,
                                  cl_dps_m = NULL,
                                  cl_dps_w = NULL,
                                  cl_dps_h = NULL,
                                  cl_std,
                                  cl_skw,
                                  cl_kurt,
                                  .for = c("wfhz", "mfaz")) {
  ## Enforce options in `.for` ----
  .for <- match.arg(.for)

  switch(.for,
    "wfhz" = {
      qs <- sum(
        score_std_flags(cl_flags),
        score_agesexr_dps(cl_sex),
        score_agesexr_dps(cl_age),
        score_agesexr_dps(cl_dps_w),
        score_agesexr_dps(cl_dps_h),
        score_std_flags(cl_std),
        score_skewkurt(cl_skw),
        score_skewkurt(cl_kurt)
      )
    },
    "mfaz" = {
      sum(
        score_std_flags(cl_flags),
        score_agesexr_dps(cl_sex),
        score_agesexr_dps(cl_age),
        score_agesexr_dps(cl_dps_m),
        score_std_flags(cl_std),
        score_skewkurt(cl_skw),
        score_skewkurt(cl_kurt)
      )
    }
  )
}
