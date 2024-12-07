#'
#'
#' @keywords internal
#'
#'
complex_survey_estimates_combined <- function(df,
                                              wt = NULL,
                                              edema = NULL,
                                              .by) {
  ## Defuse arguments ----
  wt <- enquo(wt)
  edema <- enquo(edema)

  ## Defines case based on the availability of edema ----
  if (!quo_is_null(edema)) {
    ### Case definition when `edema` is not null ----
    df <- with(
      df,
      define_wasting(df,
        zscore = .data$wfhz,
        muac = .data$muac,
        edema = !!edema,
        .by = "combined"
      ) |>
        dplyr::mutate(
          cflags = ifelse(.data$flag_wfhz == 1 | .data$flag_mfaz == 1, 1, 0)
        )
    )
  } else {
    ### Case definition when `edema` is null ----
    df <- with(
      df,
      define_wasting(df,
        zscore = .data$wfhz,
        muac = .data$muac,
        .by = "combined"
      ) |>
        dplyr::mutate(
          cflags = ifelse(.data$flag_wfhz == 1 | .data$flag_mfaz == 1, 1, 0)
        )
    )
  }

  ## Create survey object ----
  if (!quo_is_null(wt)) {
    srvy <- srvyr::as_survey_design(
      .data = df,
      ids = .data$cluster,
      pps = "brewer",
      variance = "YG",
      weights = !!wt
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
  p <- dplyr::group_by(.data = srvy, {{ .by }}) |>
    dplyr::filter(.data$cflags == 0) |>
    dplyr::summarise(
      dplyr::across(
        .data$cgam:.data$cmam,
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
      wt_pop = sum(srvyr::cur_svy_wts())
    )
  p
}



#'
#' Estimate the prevalence of combined wasting
#'
#' @description
#' Estimate the prevalence of wasting based on the combined case-definition of
#' weight-for-height z-scores (WFHZ), MUAC and/or edema. The function allows 
#' users to estimate prevalence in accordance with complex sample design 
#' properties such as accounting for survey sample weights when needed or 
#' applicable. The quality of the data is first evaluated by calculating and 
#' rating the standard deviation of WFHZ and MFAZ and the p-value of the age 
#' ratio test. Prevalence is calculated only when all tests are rated as not 
#' problematic. If any of the tests rate as problematic, no estimation is done 
#' and an NA value is returned. Outliers are detected in both WFHZ and MFAZ 
#' datasets based on SMART flagging criteria. Identified outliers are then 
#' excluded before prevalence estimation is performed.
#'
#' @param df A `tibble` object produced by sequential application of the
#' [mw_wrangle_wfhz()] and [mw_wrangle_muac()]. Note that MUAC values in `df` 
#' must be in millimeters unit after using [mw_wrangle_muac()]. Also, `df`
#' must have a variable called `cluster` which contains the primary sampling
#' unit identifiers.
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
#' @returns A summary `tibble` for the descriptive statistics about combined 
#' wasting.
#'
#' @details
#' A concept of *combined flags* is introduced in this function. Any observation
#' that is flagged for either `flag_wfhz` or `flag_mfaz` is flagged under a new
#' variable named `cflags` added to `df`. This ensures that all flagged 
#' observations from both WFHZ and MFAZ data are excluded from the prevalence 
#' analysis.
#'
#' | **flag_wfhz** | **flag_mfaz** | **cflags** |
#' | :---: | :---: | :---: |
#' | 1 | 0  | 1 |
#' | 0 | 1  | 1 |
#' | 0 | 0  | 0 |
#'
#' @examples
#' ## When .by and wt are set to NULL ----
#' mw_estimate_prevalence_combined(
#'   df = anthro.02,
#'   wt = NULL,
#'   edema = edema,
#'   .by = NULL
#' )
#'
#' ## When wt is not set to NULL ----
#' mw_estimate_prevalence_combined(
#'   df = anthro.02,
#'   wt = wtfactor,
#'   edema = edema,
#'   .by = NULL
#' )
#'
#' @export
#'
#'
mw_estimate_prevalence_combined <- function(df,
                                            wt = NULL,
                                            edema = NULL,
                                            .by = NULL) {
  ## Defuse argument `.by` ----
  .by <- enquo(.by)

  ## Enforce measuring unit is in "mm" ----
  if (any(grepl("\\.", df$muac))) {
    stop("MUAC values must be in millimeters. Please try again.")
  }

  ## Empty vector to store results ----
  results <- list()

  if (!quo_is_null(.by)) {
    ## Rate standard deviation and set MUAC analysis path ----
    x <- dplyr::summarise(
      .data = df,
      std_wfhz = rate_std(
        stats::sd(
          remove_flags(as.numeric(.data$wfhz), "zscores"), 
          na.rm = TRUE
        )
      ),
      age_ratio = rate_agesex_ratio(
        mw_stattest_ageratio(.data$age, .expectedP = 0.66)$p
      ),
      std_mfaz = rate_std(
        stats::sd(
          remove_flags(as.numeric(.data$mfaz), "zscores"), 
          na.rm = TRUE)
      ),
      muac_analysis_path = set_analysis_path(.data$age_ratio, .data$std_mfaz),
      .by = !!.by
    )
  } else {
    ## Rate standard deviation and set MUAC analysis path ----
    x <- dplyr::summarise(
      .data = df,
      std_wfhz = rate_std(
        stats::sd(
          remove_flags(as.numeric(.data$wfhz), "zscores"), 
          na.rm = TRUE
        )
      ),
      age_ratio = rate_agesex_ratio(
        mw_stattest_ageratio(.data$age, .expectedP = 0.66)$p
      ),
      std_mfaz = rate_std(
        stats::sd(
          remove_flags(as.numeric(.data$mfaz), "zscores"), 
          na.rm = TRUE
        )
      ),
      muac_analysis_path = set_analysis_path(.data$age_ratio, .data$std_mfaz)
    )
  }

  ## Iterate over data.frame to compute prevalence according to the SD ----
  for (i in seq_len(nrow(x))) {
    if (!quo_is_null(.by)) {
      area <- dplyr::pull(x, !!.by)[i]
      data_subset <- dplyr::filter(df, !!sym(quo_name(.by)) == !!area)
    } else {
      data_subset <- df
    }

    std_wfhz <- x$std_wfhz[i]
    muac_analysis_path <- x$muac_analysis_path[i]

    if (std_wfhz != "Problematic" && muac_analysis_path == "unweighted") {
      ### Compute standard complex sample based prevalence analysis ----
      output <- complex_survey_estimates_combined(
        df = data_subset,
        wt = {{ wt }},
        edema = {{ edema }},
        .by = !!.by
      )
    } else {
      ## Add NA ----
      if (!quo_is_null(.by)) {
        output <- dplyr::summarise(
          .data = data_subset,
          cgam_p = NA_real_,
          csam_p = NA_real_,
          cmam_p = NA_real_,
          .by = !!.by
        )
      } else {
        ## Add NA ----
        output <- tibble::tibble(
          cgam_p = NA_real_,
          csam_p = NA_real_,
          cmam_p = NA_real_
        )
      }
    }
    results[[i]] <- output
  }
  ### Ensure that all categories in `.by` get added to the tibble ----
  if (!quo_is_null(.by)) {
    results <- dplyr::bind_rows(results) |>
      dplyr::relocate(.data$cgam_p, .after = .data$cgam_n) |>
      dplyr::relocate(.data$csam_p, .after = .data$csam_n) |>
      dplyr::relocate(.data$cmam_p, .after = .data$cmam_n)
  } else {
    ## Ungrouped results
    results <- dplyr::bind_rows(results)
  }

  ## Return results ----
  results
}
