#'
#' Rate the acceptability of the standard deviation and the percentage of flagged
#' data
#'
#' @description
#' Rate how much high is the standard deviation and the percentage of flagged
#' data in the dataset, hence it's acceptability.
#'
#' @param p A vector of class `double` of the proportions of flagged values in
#' the dataset.
#'
#' @param sd A vector of class `double` of the values of the standard deviation.
#'
#' @param type A choice between "wfhz", "mfaz" and "crude" for the basis on which
#' the rating should be done.
#'
#' @returns A vector of class `character` for the acceptability rate.
#'
#' @details
#' The ranges of acceptability are: "Excellent", "Good", "Acceptable", "Problematic".
#' The cut-offs for WFHZ are as in the [SMART Methodology](https://smartmethodology.org/).
#' For the MFAZ and the absolute MUAC values, the maximum acceptable limit for
#' outliers is 2%, as recommended by
#' [Bilukha, O., & Kianian, B. (2023).](https://doi.org/10.1111/mcn.13478).
#' Cut-offs for the standard deviation of the absolute MUAC values are based on the
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
#' Rate the acceptability of the age and sex ratio test p-values
#'
#' @param p A vector of class `double` of the age or sex ratio test p-values.
#'
#' @returns A vector of class `character` of the same length as `p` for the
#' acceptability rate.
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
#' Rate the acceptability of the skewness and kurtosis test results
#'
#' @param sk A vector of class `double` for skewness or kurtosis test results.
#'
#' @returns A vector of class `character` of the same length as `sk` for the
#' acceptability rate.
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
#' Rate the overall acceptability score
#'
#' @description
#' Rate the overall acceptability score into "Excellent", "Good", "Acceptable" and
#' "Problematic".
#'
#' @param df A dataset of class `data.frame` containing a vector of the overall
#' acceptability score as yielded from [compute_quality_score()].
#'
#' @returns A `data.frame` based on `df`. A new column `quality_class` for the
#' overall acceptability rate is created and added to `df`.
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
