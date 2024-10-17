#'
#' Check whether the IPC Acute Malnutrition sample size requirements were met
#'
#' @description
#' Verify whether the minimum sample size requirements for the area of analysis
#' were met, in accordance with the IPC Acute Malnutrition (IPC AMN) protocols.
#'
#' @param df A dataset of class `data.frame` to check.
#'
#' @param .group A vector of class `integer` of the cluster ID's for survey,
#' screening or site ID's for screenings and sentinel sites.
#'
#' @param .data_type A choice between "survey" for survey data, "screening" for
#' screening data or "ssite" for community-based sentinel site data.
#'
#' @returns A summarised table of three columns: `groups` for the total number
#' of unique cluster or screening or site IDs; `n_obs` for the respective total
#' number of children; and `meet_ipc` for whether the IPC AMN requirements were met.
#'
#' @details
#' [The IPC Manual](https://www.ipcinfo.org/ipcinfo-website/resources/ipc-manual/en/).
#'
#'
#' @examples
#'
#' anthro.01 |>
#' dplyr::group_by(area) |>
#' check_sample_size(
#' .group = cluster,
#' .data_type = "survey"
#' )
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
