#'
#'
#' @keywords internal
#'
#'
get_estimates <- function(df, muac, edema = NULL, .by = NULL) {
  muac <- rlang::eval_tidy(enquo(muac), df)
  edema <- rlang::eval_tidy(enquo(edema), df)

  ## Enforce class of `muac` ----
  if (!is.numeric(muac)) {
    stop(
      "`muac` should be of class numeric not ",
      class(muac), ". Try again!"
    )
  }

  ### Enforce measuring unit is in "mm" ----
  if (any(grepl("\\.", df$muac))) {
    stop("MUAC values must be in millimeters. Try again!")
  }

  ## Wasting definition including `edema` ----
  if (!is.null(edema)) {
    ### Enforce class of `edema` ----
    if (!is.character(edema)) {
      stop(
        "`edema` should be of class character not ", class(edema),
        ". Try again!"
      )
    }
    ### Enforce code values in `edema` ----
    if (!all(levels(as.factor(edema)) %in% c("y", "n"))) {
      stop('Code values in `edema` must only be "y" and "n". Try again!')
    }
    ## Wasting definition including `edema` ----
    x <- with(
      df,
      define_wasting(
        df,
        muac = muac,
        edema = edema,
        .by = "muac"
      )
    )
  } else {
    ## Wasting definition without `edema` ----
    x <- with(
      df,
      define_wasting(
        df,
        muac = muac,
        .by = "muac"
      )
    )
  }

  ## Filter out flgas ----
  x <- dplyr::filter(.data = x, .data$flag_mfaz == 0)

  ## Summarize results ----
  p <- dplyr::group_by(.data = x, {{ .by }}) |>
    dplyr::summarise(
      dplyr::across(
        .data$gam:.data$mam,
        list(
          n = \(.) sum(., na.rm = TRUE),
          p = \(.) mean(., na.rm = TRUE)
        )
      )
    )
  ## Return p ----
  p
}


#'
#' Estimate the prevalence of wasting based on MUAC for non-survey data
#'
#' @description
#' It is common to estimate prevalence of wasting from non survey data, such
#' as screenings or any other community-based surveillance systems. In such
#' situations, the analysis usually consists only in estimating the point
#' prevalence and the counts of positive cases, without necessarily estimating
#' the uncertainty. This function serves this use.
#'
#' The quality of the data is first evaluated by calculating and rating the
#' standard deviation of MFAZ and the p-value of the age ratio test. Prevalence
#' is calculated only when the standard deviation of MFAZ is not problematic. If
#' both standard deviation of MFAZ and p-value of age ratio test is not
#' problematic, straightforward prevalence estimation is performed. If standard
#' deviation of MFAZ is not problematic but p-value of age ratio test is
#' problematic, age-weighting is applied to prevalence estimation to account for
#' the over-representation of younger children in the sample. If standard
#' deviation of MFAZ is problematic, no estimation is done and an NA value is
#' returned. Outliers are detected based on SMART flagging criteria for MFAZ.
#' Identified outliers are then excluded before prevalence  estimation is
#' performed.
#'
#' @param df A `tibble` object produced by [mw_wrangle_muac()] and
#' [mw_wrangle_age()] functions. Note that MUAC values in `df`
#' must be in millimeters unit after using [mw_wrangle_muac()]. Also, `df`
#' must have a variable called `cluster` which contains the primary sampling
#' unit identifiers.
#'
#' @param muac A `numeric` or `integer` vector of raw MUAC values. The
#' measurement unit of the values should be millimeters.
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
#' @references
#' SMART Initiative (no date). *Updated MUAC data collection tool*. Available at:
#' <https://smartmethodology.org/survey-planning-tools/updated-muac-tool/>
#'
#' @seealso [mw_estimate_prevalence_muac()] [mw_estimate_smart_age_wt()]
#'
#'
#' @examples
#' mw_estimate_prevalence_screening(
#'   df = anthro.02,
#'   muac = muac,
#'   edema = edema,
#'   .by = province
#' )
#'
#' ## With `edema` set to `NULL` ----
#' mw_estimate_prevalence_screening(
#'   df = anthro.02,
#'   muac = muac,
#'   edema = NULL,
#'   .by = province
#' )
#'
#' ## With `.by` set to `NULL` ----
#' mw_estimate_prevalence_screening(
#'   df = anthro.02,
#'   muac = muac,
#'   edema = NULL,
#'   .by = NULL
#' )
#'
#' @export
#'
mw_estimate_prevalence_screening <- function(df,
                                             muac,
                                             edema = NULL,
                                             .by = NULL) {
  ## Difuse argument `.by` ----
  .by <- enquo(.by)

  ## Empty vector type list to store results ----
  results <- list()

  ## Determine the analysis path that fits the data ----
  if (!quo_is_null(.by)) {
    path <- dplyr::group_by(.data = df, !!.by) |>
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
    path <- dplyr::summarise(
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
  for (i in seq_len(nrow(path))) {
    if (!quo_is_null(.by)) {
      area <- dplyr::pull(path, !!.by)[i]
      data_subset <- dplyr::filter(df, !!sym(quo_name(.by)) == area)
    } else {
      data_subset <- df
    }

    analysis_approach <- path$analysis_approach[i]
    if (analysis_approach == "unweighted") {
      if (!quo_is_null(.by)) {
        output <- get_estimates(
          df = data_subset, muac = {{ muac }}, edema = {{ edema }}, .by = !!.by
        )
      } else {
        output <- get_estimates(
          df = data_subset, muac = {{ muac }}, edema = {{ edema }}
        )
      }
    } else if (analysis_approach == "weighted") {
      if (!quo_is_null(.by)) {
        output <- mw_estimate_smart_age_wt(
          df = data_subset, edema = {{ edema }}, .by = !!.by
        )
      } else {
        output <- mw_estimate_smart_age_wt(
          df = data_subset, edema = {{ edema }}
        )
      }
    } else {
      ## Return NA's ----
      if (!quo_is_null(.by)) {
        output <- dplyr::summarise(
          .data = data_subset,
          gam_p = NA_real_,
          sam_p = NA_real_,
          mam_p = NA_real_,
          .by = !!.by
        )
      } else {
        ## Return NA's  ----
        output <- tibble::tibble(
          gam_p = NA_real_,
          sam_p = NA_real_,
          mam_p = NA_real_
        )
      }
    }

    results[[i]] <- output
  }

  ### Ensure that all categories in `.by` get added to the tibble ----
  if (!quo_is_null(.by)) {
    results <- dplyr::bind_rows(results) |>
      dplyr::relocate(.data$gam_p, .after = .data$gam_n) |>
      dplyr::relocate(.data$sam_p, .after = .data$sam_n) |>
      dplyr::relocate(.data$mam_p, .after = .data$mam_n)
  } else {
    ## Non-grouped results
    results <- dplyr::bind_rows(results)
  }

  ## Return results ----
  results
}
