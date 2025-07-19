#'
#'
#' @keywords internal
#'
#'
complex_survey_estimates_combined <- function(df,
                                              wt = NULL,
                                              edema = NULL,
                                              ...) {
  ## Defuse arguments ----
  wt <- rlang::enquo(wt)
  edema <- rlang::enquo(edema)
  .by <- rlang::enquos(...)

  ## Defines case based on the availability of edema ----
  df <- define_wasting(df,
    zscores = .data$wfhz,
    muac = .data$muac,
    edema = !!edema,
    .by = "combined"
  ) |>
    dplyr::mutate(
      cflags = ifelse(.data$flag_wfhz == 1 | .data$flag_mfaz == 1, 1, 0)
    ) |>
    dplyr::filter(
      .data$cflags == 0
    )

  ## Create a survey object for a weighted analysis ----
  srvy <- df |>
    srvyr::group_by(!!!.by) |>
    srvyr::as_survey_design(
      ids = .data$cluster,
      pps = "brewer",
      variance = "YG",
      weights = !!wt
    )

  ## Summarise prevalence ----
  p <- srvyr::summarise(
    srvy,
    srvyr::across(
      .data$cgam:.data$cmam,
      list(
        n = ~ sum(.x, na.rm = TRUE),
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
#' @param ... A vector of class `character`, specifying the categories for which
#' the analysis should be summarised for. Usually geographical areas. More than
#' one vector can be specified.
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
#' ## When wt are set to NULL ----
#' mw_estimate_prevalence_combined(
#'   df = anthro.02,
#'   wt = NULL,
#'   edema = edema
#' )
#'
#' ## When `wt` is not set to NULL ----
#' mw_estimate_prevalence_combined(
#'   df = anthro.02,
#'   wt = wtfactor,
#'   edema = edema
#' )
#'
#' @export
#'
#'
mw_estimate_prevalence_combined <- function(df,
                                            wt = NULL,
                                            edema = NULL,
                                            ...) {
  ## Capture grouping vars ----
  .by <- rlang::enquos(...)

  ## Enforce measuring unit is in "mm" ----
  if (any(grepl("\\.", df$muac))) {
    stop("MUAC values must be in millimeters. Please try again.")
  }

  ## Empty vector to store results ----
  results <- list()

  ## Apply grouping as needed ----
  if (length(.by) > 0) df <- dplyr::group_by(df, !!!.by)

  ## Rate standard deviation and set MUAC analysis path ----
  x <- dplyr::summarise(
    .data = df,
    std_wfhz = rate_std(stats::sd(remove_flags(as.numeric(.data$wfhz), "zscores"), na.rm = TRUE)),
    age_ratio = rate_agesex_ratio(mw_stattest_ageratio(.data$age, .expectedP = 0.66)$p),
    std_mfaz = rate_std(stats::sd(remove_flags(as.numeric(.data$mfaz), "zscores"), na.rm = TRUE)),
    muac_analysis_path = set_analysis_path(.data$age_ratio, .data$std_mfaz),
    .groups = "drop"
  )

  ## Iterate over data.frame to compute prevalence according to the SD ----
  for (i in seq_len(nrow(x))) {
    if (length(.by) > 0) {
      vals <- purrr::map(.by, ~ dplyr::pull(x, !!.x)[i])
      exprs <- purrr::map2(.by, vals, ~ rlang::expr(!!rlang::get_expr(.x) == !!.y))
      data_subset <- dplyr::filter(df, !!!exprs)
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
        !!!.by
      )
    } else {
      ## Add NA ----
      if (length(.by) > 0) {
        output <- data_subset |>
          dplyr::group_by(!!!.by) |>
          dplyr::summarise(
            cgam_p = NA_real_,
            csam_p = NA_real_,
            cmam_p = NA_real_
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

  ### Relocate variables ----
  results <- dplyr::bind_rows(results)
  .df <- if (any(names(results) %in% c("gam_n"))) {
    results |>
      dplyr::relocate(.data$gam_p, .after = .data$gam_n) |>
      dplyr::relocate(.data$sam_p, .after = .data$sam_n) |>
      dplyr::relocate(.data$mam_p, .after = .data$mam_n)
  } else {
    results
  }
  .df
}
