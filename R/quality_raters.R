#'
#' Rate the acceptability of the proportion of flagged records
#'
#' @description
#' Rate the acceptability of the proportion of flagged records in WFHZ, MFAZ,
#' and raw MUAC data following the SMART methodology criteria.
#'
#' @param p A vector of class `double` of the proportions of flagged records in 
#' the data set.
#'
#' @param .in Specifies the data set where the rating should be done. Can be 
#' "wfhz", "mfaz", or "raw_muac". Default to "wfhz".
#'
#' @returns A vector of class `factor` with the same length as `p` for the
#' acceptability rate.
#'
#' @keywords internal
#'
rate_propof_flagged <- function(p, .in = c("mfaz", "wfhz", "raw_muac")) {
  ## Enforce options of `.in` ----
  .in <- match.arg(.in)

  ## Enforce the class of `p` ----
  if (!is.double(p)) {
    stop(
      "`p` must be of class double not ", class(p), ". Please try again."
    )
  }

  ## Rate the acceptability of the proportion of flagged records ----
  if (.in == "mfaz" | .in == "raw_muac") {
    ## In MFAZ or WFHZ ----
    x <- cut(
      x = p,
      breaks = c(0, 0.01, 0.015, 0.02, Inf),
      labels = c("Excellent", "Good", "Acceptable", "Problematic"),
      include.lowest = TRUE,
      right = TRUE
    )
  }

  if (.in == "wfhz") {
    ## In raw MUAC values ----
    x <- cut(
      x = p,
      breaks = c(0, 0.025, 0.05, 0.075, Inf),
      labels = c("Excellent", "Good", "Acceptable", "Problematic"),
      include.lowest = TRUE,
      right = TRUE
    )
  }

  ## Return x ----
  x
}



#'
#' Rate the acceptability of the standard deviation
#'
#' @description
#' Rate the acceptability of the standard deviation of WFHZ, MFAZ, and raw MUAC 
#' data. Rating follows the SMART methodology criteria.
#'
#' @param sd A vector of class `double` of standard deviation values from the 
#' data set.
#'
#' @param .of Specifies the data set to which the rating should be done. Can be
#' "wfhz", "mfaz", or "raw_muac".
#'
#' @returns A vector of class `factor` of the same length as `sd` for the
#' acceptability rate.
#'
#' @keywords internal
#'
#'
rate_std <- function(sd, .of = c("zscores", "raw_muac")) {
  ## Enforce options of `.of` ----
  .of <- match.arg(.of)

  ## Enforce the class of `sd` ----
  if (!is.double(sd)) {
    stop(
      "`sd` must be of class double not ", class(sd), ". Please try again."
    )
  }

  if (.of == "zscores") {
    ## Rate the standard deviation of z-scores ----
    x <- dplyr::case_when(
      sd > 0.9 & sd < 1.1 ~ "Excellent",
      sd > 0.85 & sd < 1.15 ~ "Good",
      sd > 0.8 & sd < 1.20 ~ "Acceptable",
      TRUE ~ "Problematic"
    )
  }

  if (.of == "raw_muac") {
    ## Rate the standard deviation of raw MUAC values ----
    x <- cut(
      x = sd,
      breaks = c(-Inf, 13, 14, 15, Inf),
      labels = c("Excellent", "Acceptable", "Poor", "Problematic"),
      include.lowest = FALSE,
      right = FALSE
    )
  }

  ## Return x ----
  x
}


#'
#' Rate the acceptability of the age and sex ratio test p-values
#'
#' @param p A vector of class `double` of the age or sex ratio test p-values.
#'
#' @returns A `character` vector with the same length as `p` for the 
#' acceptability rate.
#'
#' @keywords internal
#'
rate_agesex_ratio <- function(p) {
  ## Enforce the class of `p` ----
  if (!is.double(p)) {
    stop(
      "`p` must be of class double not ", class(p), ". Please try again."
    )
  }

  ## Rate ----
  dplyr::case_when(
    p > 0.1 ~ "Excellent",
    p > 0.05 ~ "Good",
    p > 0.001 ~ "Acceptable",
    TRUE ~ "Problematic"
  )
}


#'
#' Rate the acceptability of the skewness and kurtosis test results
#'
#' @param sk A vector of class `double` for skewness or kurtosis test results.
#'
#' @returns A vector of class `factor` with the same length as `sk` for the
#' acceptability rate.
#'
#' @keywords internal
#'
rate_skewkurt <- function(sk) {
  ## Enforce the class of `sk` ----
  if (!is.double(sk)) {
    stop(
      "`sk` must be of class double not ", class(sk), ". Please try again."
    )
  }

  ## Rate ----
  cut(
    x = sk,
    breaks = c(-Inf, 0.2, 0.4, 0.6, Inf),
    labels = c("Excellent", "Good", "Acceptable", "Problematic"),
    include.lowest = FALSE,
    right = FALSE
  )
}

#'
#' Rate the overall acceptability of the data
#'
#' @description
#' Rate the overall data acceptability score into "Excellent", "Good", 
#' "Acceptable" or "Problematic".
#'
#' @param q A `numeric` or `integer` vector of data acceptability scores.
#'
#' @returns A vector of class `factor` with the same length as `q` of overall
#' rate of acceptability of the data.
#'
#' @keywords internal
#'
rate_overall_quality <- function(q) {
  ## Enforce the class of `q` ----
  if (!(is.numeric(q)) | is.integer(q)) {
    stop(
      "`q` must be of class numeric or integer not ", class(q), 
      ". Please try again."
    )
  }

  ## Rate ----
  cut(
    x = q,
    breaks = c(0, 9, 14, 24, Inf),
    labels = c("Excellent", "Good", "Acceptable", "Problematic"),
    include.lowest = TRUE,
    right = TRUE
  )
}
