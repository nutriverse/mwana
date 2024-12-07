#'
#' Score the acceptability rating of the check results that constitutes the
#' plausibility check suite
#'
#' @description
#' Attribute a score, also known as *penalty point*, for a given rate of 
#' acceptability of the standard deviation, proportion of flagged records, 
#' age and sex ratio, skewness, kurtosis and digit preference score check 
#' results. The scoring criteria and thresholds follows the standards in the SMART
#' plausibility check.
#'
#' @param x A `character` vector of the acceptability rate of a given check.
#' '
#' @returns An `integer` vector with the same length as `x` of the acceptability 
#' score.
#'
#' @references
#' SMART Initiative (2017). *Standardized Monitoring and Assessment for Relief
#' and Transition*. Manual 2.0. Available at: <https://smartmethodology.org>.
#'
#' @rdname scorer
#'
#' @keywords internal
#'
score_std_flags <- function(x) {
  ## Enforce the class of `x` ----
  if (!(is.character(x) | is.factor(x))) {
    stop(
      "`x` must be of class character or factor not ", class(x), 
      ". Please try again."
    )
  }

  ## Score ----
  dplyr::case_when(
    x == "Excellent" ~ 0,
    x == "Good" ~ 5,
    x == "Acceptable" ~ 10,
    x == "Problematic" ~ 20
  )
}


#'
#' @rdname scorer
#'
#' @keywords internal
#'
score_agesexr_dps <- function(x) {
  ## Enforce the class of `x` ----
  if (!(is.character(x) | is.factor(x))) {
    stop(
      "`x` must be of class character or factor not ", 
      class(x), ". Please try again."
    )
  }

  ## Score ----
  dplyr::case_when(
    x == "Excellent" ~ 0,
    x == "Good" ~ 2,
    x == "Acceptable" ~ 4,
    x == "Problematic" ~ 10
  )
}

#'
#' @rdname scorer
#'
#' @keywords internal
#'
score_skewkurt <- function(x) {
  ## Enforce the class of `x` ----
  if (!(is.character(x) | is.factor(x))) {
    stop(
      "`x` must be of class character or factor not ", 
      class(x), ". Please try again."
    )
  }

  ## Score ----
  dplyr::case_when(
    x == "Excellent" ~ 0,
    x == "Good" ~ 1,
    x == "Acceptable" ~ 3,
    x == "Problematic" ~ 5
  )
}

#'
#'
#' Get the overall acceptability score from the acceptability rate scores
#'
#' @param .for A choice between "wfhz" and "mfaz" for the type of scorer to
#' apply. Default is "wfhz".
#'
#' @returns A `numeric` value for the overall data quality (acceptability) 
#' score.
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
