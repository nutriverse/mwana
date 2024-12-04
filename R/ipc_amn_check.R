#'
#' Check whether sample size requirements for IPC Acute Malnutrition (IPC AMN)
#' analysis are met
#'
#' @description
#' Data for estimating the prevalence of acute malnutrition used in the IPC AMN
#' can come from different sources: surveys, screenings or community-based
#' surveillance systems. The IPC has set minimum sample size requirements for 
#' each  source. This function verifies whether these requirements are met.
#'
#' @param df A `data.frame` object to check.
#'
#' @param cluster A vector of class `integer` or `character` of unique cluster 
#' or screening or sentinel site identifiers. If a `character` vector, ensure 
#' that each unique name represents one location. If `cluster` is not of class
#' `integer` or `character`, an error message will be returned indicating the 
#' type of mismatch.
#'
#' @param .source The source of evidence. A choice between "survey" for
#' representative survey data at the area of analysis; "screening" for
#' screening data; "ssite" for community-based sentinel site data. Default value
#' is "survey".
#'
#' @returns A single row summary `tibble` with 3 columns containing
#' check results for: 
#' 
#' - `n_clusters` - the total number of unique clusters or
#' screening or site identifiers; 
#' - `n_obs` - the corresponding total number of children in the data set; and,
#' - `meet_ipc` - whether the IPC AMN requirements were met.
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
  ## Defuse and evaluate arguments ----
  cluster <- rlang::eval_tidy(enquo(cluster), df)

  ## Enforce the options in `.source` ----
  .source <- match.arg(.source)

  ## Enforce the class of `cluster` ----
  if (!is(cluster, "character") & !is(cluster, "integer")) {
    stop(
      "`cluster` must be of class `integer` or `character` not ", 
      shQuote(class(cluster)), 
      ". Please try again."
    )
  }

  ## Summarize ----
  df <- dplyr::summarise(
    .data = df,
    n_clusters = dplyr::n_distinct({{ cluster }}),
    n_obs = dplyr::n(),
    meet_ipc = dplyr::case_when(
      .source == "survey" & n_clusters >= 25 ~ "yes",
      .source == "screening" & n_clusters >= 3 & n_obs >= 600 ~ "yes",
      .source == "ssite" & n_clusters >= 5 & n_obs >= 200 ~ "yes",
      .default = "no"
    )
  )
  
  ## Return tibble ----
  tibble::as_tibble(df)
}
