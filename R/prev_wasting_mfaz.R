#'
#'
#' @keywords internal
#'
#'
complex_survey_estimates_mfaz <- function(df,
                                          wt = NULL,
                                          edema = NULL,
                                          .by) {
  ## Difuse arguments ----
  wt <- enquo(wt)
  edema <- enquo(edema)

  ## Defines case based on the availability of edema ----
  if (!quo_is_null(edema)) {
    ## When edema is available ----
    df <- with(
      df,
      define_wasting(df, zscores = .data$mfaz, edema = !!edema, .by = "zscores")
    )
  } else {
    ## When edema is not available ----
    df <- with(
      df,
      define_wasting(df, zscores = .data$mfaz, .by = "zscores")
    )
  }

  ## Create a survey object ----
  if (!is.null(wt)) {
    srvy <- srvyr::as_survey_design(
      .data = df,
      ids = .data$cluster,
      pps = "brewer",
      variance = "YG",
      weights = {{ wt }}
    )
  } else {
    srvy <- srvyr::as_survey_design(
      .data = df,
      ids = .data$cluster,
      pps = "brewer",
      variance = "YG"
    )
  }

  ## Summarise prevalence ----
  p <- srvyr::group_by(.data = srvy, {{ .by }}) |>
    srvyr::filter(.data$flag_mfaz == 0) |>
    srvyr::summarise(
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
#' @param .by A `character` or `numeric` vector of the geographical areas
#' or identifiers for where the data was collected and for which the analysis
#' should be summarised for.
#'
#' @returns A summary `tibble` for the descriptive statistics about wasting.
#'
#' @examples
#' ## When .by = NULL ----
#' mw_estimate_prevalence_mfaz(
#'   df = anthro.04,
#'   wt = NULL,
#'   edema = edema,
#'   .by = NULL
#' )
#'
#' ## When .by is not set to NULL ----
#' mw_estimate_prevalence_mfaz(
#'   df = anthro.04,
#'   wt = NULL,
#'   edema = edema,
#'   .by = province
#' )
#'
#' @export
#'
mw_estimate_prevalence_mfaz <- function(df,
                                        wt = NULL,
                                        edema = NULL,
                                        .by = NULL) {
  ## Defuse argument .by ----
  .by <- enquo(.by)

  ## Empty vector ----
  results <- list()

  if (!quo_is_null(.by)) {
    ## Check standard deviation ----
    x <- dplyr::summarise(
      .data = df,
      std = rate_std(
        stats::sd(remove_flags(.data$mfaz, "zscores"), na.rm = TRUE)
      ),
      .by = !!.by
    )
  } else {
    ## Check standard deviation ----
    x <- dplyr::summarise(
      .data = df,
      std = rate_std(
        stats::sd(remove_flags(.data$mfaz, "zscores"), na.rm = TRUE)
      )
    )
  }

  ## Iterate over data frame to compute prevalence according to the SD ----
  for (i in seq_len(nrow(x))) {
    if (!quo_is_null(.by)) {
      area <- dplyr::pull(x, !!.by)[i]
      data_subset <- dplyr::filter(df, !!sym(quo_name(.by)) == !!area)
    } else {
      data_subset <- df
    }

    std <- x$std[i]
    if (std != "Problematic") {
      ### Compute standard complex sample based prevalence analysis ----
      result <- complex_survey_estimates_mfaz(
        data_subset, {{ wt }}, {{ edema }}, !!.by
      )
    } else {
      ### Compute grouped PROBIT based prevalence ----
      if (!quo_is_null(.by)) {
        result <- estimate_probit_prevalence(data_subset, !!.by, .for = "mfaz")
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
