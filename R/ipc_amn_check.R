#'
#' Check whether IPC Acute Malnutrition (IPC AMN) sample size requirements were met
#'
#' @description
#' Evidence on the prevalence of acute malnutrition used in the IPC AMN
#' can come from different sources: surveys, screenings or community-based
#' surveillance system. The IPC set minimum sample size requirements
#' for each source. This function helps in verifying whether those requirements
#' were met or not depending on the source.
#'
#' @param df A data set object of class `data.frame` to check.
#'
#' @param cluster A vector of class `integer` or `character` of unique cluster or
#' screening or sentinel site IDs. If a `character` vector, ensure that names are
#' correct and each name represents one location for accurate counts. If the class
#' does not match the above expected type, the function will stop execution and
#' return an error message indicating the type of mismatch.
#'
#' @param .source The source of evidence. A choice between "survey" for
#' representative survey data at the area of analysis; "screening" for
#' screening data; "ssite" for community-based sentinel site data.
#'
#' @returns A summary table of class `data.frame`, of length 3 and width 1, for
#' the check results. `n_clusters` is for the total number of unique clusters or
#' screening or site IDs; `n_obs` for the correspondent total number of children
#' in the data set; and `meet_ipc` for whether the IPC AMN requirements were met.
#'
#' @references
#' IPC Global Partners. 2021. *Integrated Food Security Phase Classification*
#' *Technical Manual Version 3.1.Evidence and Standards for Better Food Security*
#' *and Nutrition Decisions*. Rome. Available at:
#' <https://www.ipcinfo.org/ipcinfo-website/resources/ipc-manual/en/>.
#'
#' @examples
#' mw_check_ipcamn_ssreq(
#'   df = anthro.01,
#'   cluster = cluster,
#'   .source = "survey"
#' )
#'
#' @export
#'
mw_check_ipcamn_ssreq <- function(df,
                                  cluster,
                                  .source = c("survey", "screening", "ssite")) {
  ## Difuse and evaluate arguments ----
  cluster <- eval_tidy(enquo(cluster), df)

  ## Enforce the options in `.source` ----
  .source <- match.arg(.source)

  ## Enforce the class of `cluster` ----
  if (!(class(cluster) %in% c("integer", "character"))) {
    stop(
      "`cluster` must be of class `integer` or `character`; not ", shQuote(class(cluster)), ". Please try again."
    )
  }

  ## Summarize ----
  df <- df |>
    summarise(
      n_clusters = n_distinct({{ cluster }}),
      n_obs = n(),
      meet_ipc = case_when(
        .source == "survey" & n_clusters >= 25 ~ "yes",
        .source == "screening" & n_clusters >= 3 & n_obs >= 600 ~ "yes",
        .source == "ssite" & n_clusters >= 5 & n_obs >= 200 ~ "yes",
        .default = "no"
      )
    )
  as_tibble(df)
}
