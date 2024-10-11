#'
#' Check if the IPC AMN sample size requirement were met
#'
#' @description
#' `check_sample_size()` verifies if the minimum sample size requirements of the
#' IPC Acute Malnutrition protocols are met in a given area of analysis.
#'
#' @param df A data frame containing the required variables.
#'
#' @param .group A vector containing the primary sampling unit (PSU) ID's. Usually and
#' ideally a numeric vector, but sometimes this may present itself as a character.
#' Either way, `check_sample_size()` will work accordingly.
#'
#' @param .data_type A choice between "survey" for survey data, "screening" for
#' screening data or "ssite" for community-based sentinel site data.
#'
#' @returns By default, a summary table of one row and three additional columns
#' are returned. Column `groups` and `n_obs` hold the total number of unique
#' PSU's and children respectively, and `meet_ipc` tells whether the IPC AMN
#' sample size requirements were met.
#'
#' @details
#' Use dplyr::group_by() before `check_sample_size()` to get a summary for each
#' unique survey or screening location from your data.
#'
#' @examples
#' check_sample_size(anthro.01, .group = cluster, .data_type = "survey")
#'
#' @export
#'
check_sample_size <- function(df,
                              .group,
                              .data_type = c("survey", "screening", "ssite")) {

  ## Match arguments ----
  data_type <- match.arg(.data_type)

  ## Summarize unique PSU's and total observations per PSU ----
  df <- df |>
    summarise(
      groups = n_distinct({{ .group }}),
      n_obs = n(),
      meet_ipc = case_when(
        data_type == "survey" & groups >= 25 ~ "yes",
        data_type == "screening" & groups >= 3 & n_obs >= 600 ~ "yes",
        data_type == "ssite" & groups >= 5 & n_obs >= 200 ~ "yes",
        .default = "no"
    )
  )
  tibble::as_tibble(df)
}
