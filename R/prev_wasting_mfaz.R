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
      define_wasting(
        df,
        zscores = .data$mfaz,
        edema = !!edema,
        .by = "zscores"
      )
    )
  } else {
    ## When edema is not available ----
    df <- with(
      df,
      define_wasting(
        df,
        zscores = .data$mfaz,
        .by = "zscores"
      )
    )
  }

  ## Create a survey object ----
  if (!is.null(wt)) {
    srvy <- df |>
      as_survey_design(
        ids = .data$cluster,
        pps = "brewer",
        variance = "YG",
        weights = {{ wt }}
      )
  } else {
    srvy <- df |>
      as_survey_design(
        ids = .data$cluster,
        pps = "brewer",
        variance = "YG"
      )
  }

  ## Summarise prevalence ----
  p <- srvy |>
    group_by({{ .by }}) |>
    filter(.data$flag_mfaz == 0) |>
    summarise(
      across(
        c(.data$gam:.data$mam),
        list(
          n = \(.)sum(., na.rm = TRUE),
          p = \(.)survey_mean(.,
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
#' muac-for-age and/or bilateral edema. The function allows users to
#' get the prevalence estimates calculated in accordance with the complex sample
#' design properties; this includes applying survey weights when needed or applicable.
#'
#' Before estimating, the function evaluates the quality of data by calculating
#' and rating the standard deviation of z-scores of MFAZ. If rated as problematic,
#' the prevalence is estimated based on the PROBIT method.
#'
#' Outliers are detected based on SMART flags and get excluded prior prevalence analysis.
#'
#' @param df A data set object of class `data.frame` to use. This must have been
#' wrangled using this package's wrangling function for MUAC data. The function
#' uses a variable name called `cluster` where the primary sampling unit IDs
#' are stored. Make sure to rename your cluster ID variable to `cluster`, otherwise
#' the function will error and terminate the execution.
#'
#' @param wt A vector of class `double` of the final survey weights. Default is
#'  `NULL` assuming a self weighted survey, as in the ENA for SMART software;
#'  otherwise, when a vector of weights if supplied, weighted analysis is done.
#'
#' @param edema A vector of class `character` of edema. Code should be
#' "y" for presence and "n" for absence of bilateral edema. Default is `NULL`.
#'
#' @param .by A vector of class `character` or `numeric` of the geographical areas
#' or respective IDs for where the data was collected and for which the analysis
#' should be summarized at.
#'
#' @returns A summarized table of class `data.frame` of the descriptive
#' statistics about wasting.
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
  ## Difuse argument .by ----
  .by <- enquo(.by)

  ## Empty vector ----
  results <- list()

  if (!quo_is_null(.by)) {
    ## Check standard deviation ----
    x <- df |>
      summarise(
        std = rate_std(sd(remove_flags(.data$mfaz, "zscores"), na.rm = TRUE)),
        .by = !!.by
      )
  } else {
    ## Check standard deviation ----
    x <- df |>
      summarise(
        std = rate_std(sd(remove_flags(.data$mfaz, "zscores"), na.rm = TRUE))
      )
  }

  ## Iterate over data frame to compute prevalence according to the SD ----
  for (i in seq_len(nrow(x))) {
    if (!quo_is_null(.by)) {
      area <- pull(x, !!.by)[i]
      data <- filter(df, !!sym(quo_name(.by)) == !!area)
    } else {
      data <- df
    }

    std <- x$std[i]
    if (std != "Problematic") {
      ### Compute standard complex sample based prevalence analysis ----
      result <- complex_survey_estimates_mfaz(data, {{ wt }}, {{ edema }}, !!.by)
    } else {
      ### Compute grouped PROBIT based prevalence ----
      if (!quo_is_null(.by)) {
        result <- estimate_probit_prevalence(data, !!.by, .for = "mfaz")
      } else {
        ### Compute PROBIT based prevalence ----
        result <- estimate_probit_prevalence(data, .for = "mfaz")
      }
    }
    results[[i]] <- result
  }
  bind_rows(results) |>
    relocate(.data$gam_p, .after = .data$gam_n) |>
    relocate(.data$sam_p, .after = .data$sam_n) |>
    relocate(.data$mam_p, .after = .data$mam_n)
}
