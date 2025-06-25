#'
#'
#' @keywords internal
#'
#'
set_analysis_path <- function(ageratio_class, sd_class) {
  ## Enforce class of both arguments ----
  ageratio_class <- as.character(ageratio_class)
  sd_class <- as.character(sd_class)

  ## Set the analysis path ----
  dplyr::case_when(
    ageratio_class == "Problematic" & sd_class != "Problematic" ~ "weighted",
    ageratio_class != "Problematic" & sd_class == "Problematic" ~ "missing",
    ageratio_class == "Problematic" & sd_class == "Problematic" ~ "missing",
    .default = "unweighted"
  )
}


#'
#'
#' @keywords internal
#'
#'
smart_age_weighting <- function(muac,
                                age,
                                edema = NULL,
                                .form = c("sam", "mam")) {
  ## Match arguments ----
  .form <- match.arg(.form)

  if (!is.null(edema)) {
    ### Define cases ----
    nut_status <- smart_tool_case_definition(muac = muac, edema = {{ edema }})

    ### Estimate age-weighted prevalence as per SMART MUAC Tool ----
    age_group <- ifelse(age < 24, "under_2", "over_2")
    nut_U2 <- ifelse(age_group == "under_2" & nut_status == .form, 1, 0)
    nut_O2 <- ifelse(age_group == "over_2" & nut_status == .form, 1, 0)
    p <- mean(nut_U2, na.rm = TRUE) + (2 * mean(nut_O2, na.rm = TRUE)) / 3
  } else {
    ### Define cases ----
    nut_status <- smart_tool_case_definition(muac)

    ### Estimate age-weighted prevalence as per SMART MUAC Tool ----
    age_group <- ifelse(age < 24, "under_2", "over_2")
    nut_U2 <- ifelse(age_group == "under_2" & nut_status == .form, 1, 0)
    nut_O2 <- ifelse(age_group == "over_2" & nut_status == .form, 1, 0)
    p <- mean(nut_U2, na.rm = TRUE) + (2 * mean(nut_O2, na.rm = TRUE)) / 3
  }
  p
}


#'
#'
#' @keywords internal
#'
#'
complex_survey_estimates_muac <- function(df,
                                          wt = NULL,
                                          edema = NULL,
                                          .by = NULL) {
  ## Difuse arguments ----
  wt <- enquo(wt)
  edema <- enquo(edema)

  ## Defines case based on the availability of edema ----
  if (!quo_is_null(edema)) {
    df <- define_wasting(
      df = df,
      muac = .data$muac,
      edema = !!edema,
      .by = "muac"
    )
  } else {
    df <- define_wasting(
      df = df,
      muac = .data$muac,
      .by = "muac"
    )
  }

  ## Filter out flags ----
  df <- dplyr::filter(.data = df, .data$flag_mfaz == 0)

  ## Create a survey object for a weighted analysis ----
  if (!is.null(wt)) {
    srvy <- srvyr::as_survey_design(
      .data = df,
      ids = .data$cluster,
      pps = "brewer",
      variance = "YG",
      weights = !!wt
    )
  } else {
    ## Create a survey object for an unweighted analysis ----
    srvy <- srvyr::as_survey_design(
      .data = df,
      ids = .data$cluster,
      pps = "brewer",
      variance = "YG"
    )
  }
  #### Summarise prevalence ----
  p <- srvyr::group_by(.data = srvy, {{ .by }}) |>
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
      wt_pop = sum(srvyr::cur_svy_wts())
    )
  p
}


#'
#' Estimate the prevalence of wasting based on MUAC for survey data
#'
#' @description
#'
#' Estimate the prevalence of wasting based on MUAC and/or nutritional edema.
#' The function allows users to estimate prevalence in accordance with complex
#' sample design properties such as accounting for survey sample weights when
#' needed or applicable. The quality of the data is first evaluated by
#' calculating and rating the standard deviation of MFAZ and the p-value of the
#' age ratio test. Prevalence is calculated only when the standard deviation of
#' MFAZ is not problematic. If both standard deviation of MFAZ and p-value of
#' age ratio test is not problematic, straightforward prevalence estimation is
#' performed. If standard deviation of MFAZ is not problematic but p-value of
#' age ratio test is problematic, age-weighting is applied to prevalence
#' estimation to account for the over-representation of younger children in the
#' sample. If standard deviation of MFAZ is problematic, no estimation is done
#' and an NA value is returned. Outliers are detected based on SMART flagging
#' criteria for MFAZ. Identified outliers are then excluded before prevalence
#' estimation is performed.
#'
#' @param df A `tibble` object produced by [mw_wrangle_muac()] and
#' [mw_wrangle_age()] functions. Note that MUAC values in `df`
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
#' @param raw_muac Logical. Whether outliers should be excluded based on the raw
#' MUAC values or MFAZ.
#'
#' @param .by A `character` or `numeric` vector of the geographical areas
#' or identifiers for where the data was collected and for which the analysis
#' should be summarised for.
#'
#' @returns A summary `tibble` for the descriptive statistics about combined
#' wasting.
#'
#' @references
#' SMART Initiative (no date). *Updated MUAC data collection tool*. Available at:
#' <https://smartmethodology.org/survey-planning-tools/updated-muac-tool/>
#'
#'
#' @seealso [mw_estimate_smart_age_wt()] [mw_estimate_prevalence_mfaz()]
#' [mw_estimate_prevalence_screening()]
#'
#' @examples
#' ## When .by = NULL ----
#' mw_estimate_prevalence_muac(
#'   df = anthro.04,
#'   wt = NULL,
#'   edema = edema,
#'   .by = NULL
#' )
#'
#' ## When .by is not set to NULL ----
#' mw_estimate_prevalence_muac(
#'   df = anthro.04,
#'   wt = NULL,
#'   edema = edema,
#'   .by = province
#' )
#'
#' @rdname prev_muac
#'
#' @export
#'
mw_estimate_prevalence_muac <- function(df,
                                        wt = NULL,
                                        edema = NULL,
                                        .by = NULL) {
  ## Difuse argument `.by` ----
  .by <- enquo(.by)


  ## Enforce measuring unit is in "mm" ----
  if (any(grepl("\\.", df$muac))) {
    stop("MUAC values must be in millimeters. Please try again.")
  }

  ## Empty vector type list to store results ----
  results <- list()

  if (!quo_is_null(.by)) {
    ## Evaluate the analysis path by `.by`  ----
    x <- dplyr::group_by(.data = df, !!.by) |>
      dplyr::summarise(
        age_ratio = rate_agesex_ratio(
          mw_stattest_ageratio(.data$age, .expectedP = 0.66)$p
        ),
        std = rate_std(
          stats::sd(
            remove_flags(as.numeric(.data$mfaz), "zscores"), na.rm = TRUE
          )
        ),
        analysis_approach = set_analysis_path(.data$age_ratio, .data$std),
        .groups = "drop"
      )
  } else {
    ## Evaluate the analysis path ----
    x <- dplyr::summarise(
      .data = df,
      age_ratio = rate_agesex_ratio(
        mw_stattest_ageratio(.data$age, .expectedP = 0.66)$p
      ),
      std = rate_std(
        stats::sd(
          remove_flags(as.numeric(.data$mfaz), "zscores"), na.rm = TRUE
        )
      ),
      analysis_approach = set_analysis_path(.data$age_ratio, .data$std)
    )
  }

  ## Iterate over a data frame and compute estimates as per analysis path ----
  for (i in seq_len(nrow(x))) {
    if (!quo_is_null(.by)) {
      area <- dplyr::pull(x, !!.by)[i]
      data_subset <- dplyr::filter(df, !!sym(quo_name(.by)) == area)
    } else {
      data_subset <- df
    }

    analysis_approach <- x$analysis_approach[i]

    if (analysis_approach == "unweighted") {
      ##£ Estimate PPS-based prevalence ----
      output <- complex_survey_estimates_muac(
        data_subset, {{ wt }}, {{ edema }}, !!.by
      )
    } else if (analysis_approach == "weighted") {
      ### Estimate age-weighted prevalence as per SMART MUAC tool ----
      if (!quo_is_null(.by)) {
        output <- mw_estimate_smart_age_wt(
          data_subset,
          edema = {{ edema }},
          raw_muac = FALSE,
          .by = !!.by
        )
      } else {
      ### Estimate age-weighted prevalence as per SMART MUAC tool ----
        output <- mw_estimate_smart_age_wt(
          data_subset, edema = {{ edema }}, raw_muac = FALSE
        )
      }
    } else {
      ##£ Return NA's ----
      if (!quo_is_null(.by)) {
        output <- dplyr::summarise(
          .data = data_subset,
          gam_p = NA_real_,
          sam_p = NA_real_,
          mam_p = NA_real_,
          .by = !!.by
        )
      } else {
      ### Return NA's  ----
        output <- tibble::tibble(
          gam_p = NA_real_,
          sam_p = NA_real_,
          mam_p = NA_real_
        )
      }
    }
    results[[i]] <- output
  }

  ## Ensure that all categories in `.by` get added to the tibble ----
  if (!quo_is_null(.by)) {
    results <- dplyr::bind_rows(results) |>
      dplyr::relocate(.data$gam_p, .after = .data$gam_n) |>
      dplyr::relocate(.data$sam_p, .after = .data$sam_n) |>
      dplyr::relocate(.data$mam_p, .after = .data$mam_n)
  } else {
    ### Non-grouped results ----
    results <- dplyr::bind_rows(results)
  }

  ## Return results ----
  results
}

#'
#' @examples
#' ## An application of `mw_estimate_smart_age_wt()` ----
#' .data <- anthro.04 |> subset(province == "Province 2")
#'
#' mw_estimate_smart_age_wt(
#'   df = .data,
#'   edema = edema,
#'   .by = NULL
#' )
#'
#' @rdname prev_muac
#' @export
#'

mw_estimate_smart_age_wt <- function(df, edema = NULL, raw_muac = FALSE, .by = NULL) {
  ## Defuse argument `.by` ----
  .by <- enquo(.by)

  ## Enforce measuring unit is in "mm" ----
  if (any(grepl("\\.", df$muac))) {
    stop("MUAC values must be in millimeters. Please try again.")
  }

  flag_var <- if (raw_muac) "flag_muac" else "flag_mfaz"
  df <- dplyr::filter(df, .data[[flag_var]] == 0)

  ## Summarise with or without grouping
  if (!rlang::quo_is_null(.by)) {
    df <- df |>
      dplyr::summarise(
        sam = smart_age_weighting(.data$muac, .data$age, {{ edema }}, .form = "sam"),
        mam = smart_age_weighting(.data$muac, .data$age, {{ edema }}, .form = "mam"),
        gam = .data$sam + .data$mam,
        .by = !!.by
      )
  } else {
    df <- df |>
      dplyr::summarise(
        sam = smart_age_weighting(.data$muac, .data$age, {{ edema }}, .form = "sam"),
        mam = smart_age_weighting(.data$muac, .data$age, {{ edema }}, .form = "mam"),
        gam = .data$sam + .data$mam
      )
  }

  ## Rename outputs
  df |>
    dplyr::rename(
      gam_p = .data$gam,
      sam_p = .data$sam,
      mam_p = .data$mam
    )

}