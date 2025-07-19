#'
#'
#' @keywords internal
#'
#'
complex_survey_estimates_mfaz <- function(df,
                                          wt = NULL,
                                          edema = NULL,
                                          ...) {
  ## Difuse arguments ----
  wt <- rlang::enquo(wt)
  edema <- rlang::enquo(edema)
  .by <- rlang::enquos(...)

  ## Defines case based on the availability of edema ----
  df <- define_wasting(
    df,
    zscores = .data$mfaz,
    edema = !!edema,
    .by = "zscores"
  )

  ## Filter out flags ----
  df <- dplyr::filter(.data = df, .data$flag_mfaz == 0)

  ## Create a survey object for a weighted analysis ----
  srvy <- srvyr::group_by(df, !!!.by) |>
    srvyr::as_survey_design(
      ids = .data$cluster,
      pps = "brewer",
      variance = "YG",
      weights = !!wt
    )

  ## Summarise prevalence ----
  p <- srvyr::summarise(
    .data = srvy,
    srvyr::across(
      .data$gam:.data$mam,
      list(
        n = \(.) sum(., na.rm = TRUE),
        p = \(.) srvyr::survey_mean(
          .,
          vartype = "ci",
          level = 0.95,
          deff = TRUE,
          na.rm = TRUE
        )
      )
    ),
    wt_pop = round(sum(srvyr::cur_svy_wts()))
  )
  p
}



#'
#' Estimate the prevalence of wasting based on z-scores of muac-for-age (MFAZ)
#'
#' @description
#' Calculate the prevalence estimates of wasting based on z-scores of
#' MUAC-for-age and/or bilateral edema. The function allows users to estimate
#' prevalence in accordance with complex sample design properties such as
#' accounting for survey sample weights when needed or applicable. The quality
#' of the data is first evaluated by calculating and rating the standard
#' deviation of MFAZ. Standard approach to prevalence estimation is calculated
#' only when the standard deviation of MFAZ is rated as not problematic. If
#' the standard deviation is problematic, prevalence is estimated using the
#' PROBIT estimator. Outliers are detected based on SMART flagging criteria.
#' Identified outliers are then excluded before prevalence estimation is
#' performed.
#'
#' @param df A `data.frame` object that has been produced by the
#' [mw_wrangle_age()] and [mw_wrangle_muac()] functions. The `df` should have a
#' variable named `cluster` for the primary sampling unit identifiers.
#'
#' @param wt A vector of class `double` of the survey sampling weights. Default
#' is NULL which assumes a self-weighted survey as is the case for a survey
#' sample selected proportional to population size (i.e., SMART survey sample).
#' Otherwise, a weighted analysis is implemented.
#'
#' @param edema A `character` vector for presence of nutritional edema coded as
#' "y" for presence of nutritional edema and "n" for absence of nutritional
#' edema. Default is NULL.
#'
#' @param ... A vector of class `character`, specifying the categories for which
#' the analysis should be summarised for. Usually geographical areas. More than
#' one vector can be specified.
#'
#' @returns A summary `tibble` for the descriptive statistics about wasting.
#'
#' @examples
#' ## Without grouping variables ----
#' mw_estimate_prevalence_mfaz(
#'   df = anthro.04,
#'   wt = NULL,
#'   edema = edema
#' )
#'
#' ## With grouping variables ----
#' mw_estimate_prevalence_mfaz(
#'   df = anthro.04,
#'   wt = NULL,
#'   edema = edema,
#'   province
#' )
#'
#' @export
#'
mw_estimate_prevalence_mfaz <- function(df,
                                        wt = NULL,
                                        edema = NULL,
                                        ...) {
  ## Defuse argument .by ----
  .by <- rlang::enquos(...)

  ## Empty vector ----
  results <- list()

  ## Apply grouping if needed ----
  if (length(.by) > 0) df <- dplyr::group_by(df, !!!.by)

  ## Check standard deviation ----
  x <- dplyr::summarise(
    .data = df,
    std = rate_std(stats::sd(remove_flags(.data$mfaz, "zscores"), na.rm = TRUE)),
    .groups = "keep"
  )

  ## Iterate over data frame to compute prevalence according to the SD ----
  for (i in seq_len(nrow(x))) {
    if (length(.by) > 0) {
      vals <- purrr::map(.by, ~ dplyr::pull(x, !!.x)[i])
      exprs <- purrr::map2(.by, vals, ~ rlang::expr(!!rlang::get_expr(.x) == !!.y))
      data_subset <- dplyr::filter(df, !!!exprs)
    } else {
      data_subset <- df
    }

    std <- x$std[i]
    if (std != "Problematic") {
      ### Compute standard complex sample based prevalence analysis ----
      result <- complex_survey_estimates_mfaz(
        data_subset, {{ wt }}, {{ edema }}, !!!.by
      )
    } else {
      ### Compute grouped PROBIT based prevalence ----
      if (length(.by) > 0) {
        result <- estimate_probit_prevalence(data_subset, .for = "mfaz", !!!.by)
      } else {
        ### Compute PROBIT based prevalence ----
        result <- estimate_probit_prevalence(data_subset, .for = "mfaz")
      }
    }
    results[[i]] <- result
  }
  dplyr::bind_rows(results) |>
    dplyr::relocate(.data$gam_p, .after = .data$gam_n) |>
    dplyr::relocate(.data$sam_p, .after = .data$sam_n) |>
    dplyr::relocate(.data$mam_p, .after = .data$mam_n)
}
