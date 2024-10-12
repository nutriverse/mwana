#'
#' Score the rating of proportion of flagged data, the magnitude of the standard
#' deviation, skewness, kurtosis and the p-values sex and age ratio test
#'
#' @description
#' `assign_penalty_points_flags_and_sd()` ranks the proportion of the flagged
#' values in the data and the magnitude of standard deviation based on the SMART
#' scoring criteria.
#'
#' @param x A character vector holding the test classifications for the proportion
#' of flagged data, the magnitude of the standard deviation, the p-values of the
#' age and sex ratio tests, as well as the results of skewness and kurtosis tests.
#'
#' @returns A numeric vector with the corresponding score.
#'
#' @details
#' The ranking is as in [SMART Plausibility checks](https://smartmethodology.org/).
#'
#' @rdname scorer
#'
assign_penalty_points_flags_and_sd <- function(x) {
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
assign_penalty_points_age_sex_ratio <- function(x) {
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
assign_penalty_points_skew_kurt <- function(x) {
  case_when(
    x == "Excellent" ~ 0,
    x == "Good" ~ 1,
    x == "Acceptable" ~ 3,
    x == "Problematic" ~ 5
  )
}

#'
#'
#' Get the overall quality score for WFHZ and MFAZ
#'
#' @description
#' `compute_quality_score()` calculates the overall score of the quality of the
#' data for both WFHZ and MFAZ.
#'
#' @param df A data frame containing individual test quality scores.
#'
#' @param type The method you wish to get the overall quality score for.
#' A choice between "mfaz" and "wfhz".
#'
#' @returns A vector named `"quality_score"` with the overall quality score.
#'
#'
#' @examples
#'
#' ## A sample data ----
#'
#' df <- data.frame(
#' flagged_class = "Excellent",
#' age_ratio_class = "Good",
#' sex_ratio_class = "Problematic",
#' dps_class = "Excellent",
#' sd_class = "Excellent",
#' skew_class = "Good",
#' kurt_class = "Acceptable"
#' )
#'
#' ## Apply the function ----
#' compute_quality_score(df, type = "mfaz")
#'
#' @export
#'
compute_quality_score <- function(df, type = c("mfaz", "whz")) {
  type <- match.arg(type)

  if (type == "mfaz") {

    ### Get MFAZ's quality score ----
    qscore <- df |>
      summarise(
        quality_score = sum(
          across(
            .cols = c(
              .data$flagged_class,
              .data$sd_class
              ),
            .fns = assign_penalty_points_flags_and_sd
          ),
          across(
            .cols = c(
              .data$sex_ratio_class,
              .data$age_ratio_class,
              .data$dps_class
              ),
            .fns = assign_penalty_points_age_sex_ratio
          ),
          across(
            .cols = c(
              .data$skew_class,
              .data$kurt_class
              ),
            .fns = assign_penalty_points_skew_kurt
          )
        )
      )
    qscore[["quality_score"]]

  } else {
    ### Get WHZ's quality score (REVISE)----
    qscore <- df |>
      summarise(
        quality_score = sum(
          across(
            .cols = c(
              .data$flagged_class,
              .data$sd_class
              ),
            .fns = assign_penalty_points_flags_and_sd
          ),
          across(
            .cols = c(
              .data$sex_ratio_class,
              .data$age_ratio_class,
              .data$dps_wgt_class,
              .data$dps_hgt_class
              ),
            .fns = assign_penalty_points_age_sex_ratio
          ),
          across(
            .cols = c(
              .data$skew_class,
              .data$kurt_class
              ),
            .fns = assign_penalty_points_skew_kurt
          )
        )
      )
    qscore[["quality_score"]]
  }
}

