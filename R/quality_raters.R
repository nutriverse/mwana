#'
#' Rate the proportion of flagged values in the data and the magnitude of the
#' standard deviation
#'
#' @description
#' `classify_percent_flagged()` rates how much high is the proportion of
#' of flagged data in your data set, as well as the magnitude of the standard
#' deviation. It applies for the WFHZ, the MFAZ and absolute MUAC values.
#'
#' @param p A numeric vector containing the proportions of flagged values
#'
#' @param sd A numeric vector containing values for standard deviation.
#'
#' @param type The indicator to be used for the rating. A choice between "mfaz"
#' for MFAZ, "whz" for WFHZ and "crude" for crude MUAC.
#'
#' @returns A character vector with the rating results.
#'
#' @details
#' The rating categories are: "Excellent", "Good", "Acceptable", "Problematic".
#' The cut-offs of the WFHZ are as in the [
#' SMART Methodology](https://smartmethodology.org/). As for the MFAZ and the
#' absolute MUAC values, the maximum acceptable limit is at 2%, as recommended
#' by [Bilukha, O., & Kianian, B. (2023).](https://doi.org/10.1111/mcn.13478).
#' Cut-offs for crude MUAC are based on the
#' [IPC AMN guidelines](https://www.ipcinfo.org/ipcinfo-website/resources/ipc-manual/en/).
#'
#'
#' @rdname raters
#'
classify_percent_flagged <-  function(p, type = c("mfaz", "whz", "crude")) {

  type <- match.arg(type)

  if (type == "mfaz" || type == "crude") {

    ## classify percent of outliers in MFAZ ----
    x <- cut(
      x = p,
      breaks = c(0, 0.01, 0.015, 0.02, Inf),
      labels = c("Excellent", "Good", "Acceptable", "Problematic"),
      include.lowest = TRUE,
      right = TRUE
    )
  }

  if (type == "whz") {

    ## classify percent of outliers in WHZ ----
    x <- cut(
      x = p,
      breaks = c(0, 0.025, 0.05, 0.075, Inf),
      labels = c("Excellent", "Good", "Acceptable", "Problematic"),
      include.lowest = TRUE,
      right = TRUE
    )
  }
  x
}

#'
#'
#' @rdname raters
#'
classify_sd <-  function(sd, type = c("zscore", "crude")) {

  type <- match.arg(type)

  if (type == "zscore") {

    ## Classify WHZ and MFAZ-based standard deviation ----
    x <- case_when(
      sd > 0.9 & sd < 1.1 ~ "Excellent",
      sd > 0.85 & sd < 1.15 ~ "Good",
      sd > 0.8 & sd < 1.20 ~ "Acceptable",
      TRUE ~ "Problematic"
    )
  }

  if (type == "crude") {

    ## Classify crude MUAC-based standard deviation ----
    x <- cut(
      x = sd,
      breaks = c(-Inf, 13, 14, 15, Inf),
      labels = c("Excellent", "Acceptable", "Poor", "Problematic"),
      include.lowest = FALSE,
      right = FALSE
    )
  }
  x
}


#'
#' Rate the p-values of the age and sex ratio test
#'
#' @param p A numeric vector containing the test p-values.
#'
#' @returns A character vector with the rating results.
#'
#'
classify_age_sex_ratio <- function(p) {
  case_when(
    p > 0.1 ~ "Excellent",
    p > 0.05 ~ "Good",
    p > 0.001 ~ "Acceptable",
    TRUE ~ "Problematic"
  )
}


#'
#' Rate the magnitude of skewness and kurtosis test results
#'
#' @param sk A numeric vector containing values of either skewness or kurtosis.
#'
#' @returns A character vector with the rating results.
#'
#'
classify_skew_kurt <- function(sk) {
  cut(
    x = sk,
    breaks = c(-Inf, 0.2, 0.4, 0.6, Inf),
    labels = c("Excellent", "Good", "Acceptable", "Problematic"),
    include.lowest = FALSE,
    right = FALSE
  )
}

#'
#'
#' Rate the overall data quality
#'
#' @description
#' `classify_overall_quality()` informs you about the overall quality of the data
#' by rating the overall quality score in "Excellent", "Good", "Acceptable" and
#' "Problematic".
#'
#' @param df A data frame containing a vector with the quality scores yielded
#' from [compute_quality_score()].
#'
#' @returns A character vector of the same length with a new column called
#' `quality_class`.
#'
#' @examples
#' ## A sample data ----
#'
#' df <- data.frame(
#' quality_score = 29
#' )
#'
#' ## Apply the function ----
#' classify_overall_quality(df)
#'
#' @export
#'
classify_overall_quality <- function(df) {

  qclass <- with(
    df,
    data.frame(
      quality_class <- cut(
        x = quality_score,
        breaks = c(0, 9, 14, 24, Inf),
        labels = c("Excellent", "Good", "Acceptable", "Problematic"),
        include.lowest = TRUE,
        right = TRUE
      )
    )
  )
  qclass$quality_class
}
