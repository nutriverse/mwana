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
#' Get the overall acceptability score from the acceptability classification scores
#'
#' @description
#' Calculate the total amount of penalty points based on each plausibility test
#' result acceptability classification for WFHZ and MFAZ.
#'
#' @param df A dataset object of class `data.frame` to calculate from.
#'
#' @param type A choice between "wfhz" and "mfaz" for the basis on which the
#' calculations should be made.
#'
#' @returns A `data.frame` based on `df` with a new column named `"quality_score"`
#' for the overall of acceptability (of quality) score.
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

