#'
#'
#' @keywords internal
#'
#'
get_estimates <- function(df, muac, edema = NULL, raw_muac = FALSE, ...) {
  ## Difuse arguments ----
    by <- rlang::enquos(...)
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

  ## Filter out flags ----
  flag <- if (raw_muac) "flag_muac" else "flag_mfaz"
  x <- dplyr::filter(x, .data[[flag]] == 0)

  ## Summarize results ----
  p <- dplyr::group_by(.data = x, !!!by) |>
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
#' @param age_cat A `character` vector of child's age in categories. Code values
#' should be "6-23" and "24-59".
#'
#' @param muac A `numeric` or `integer` vector of raw MUAC values. The
#' measurement unit of the values should be millimeters.
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
#'   province
#' )
#'
#' ## With `edema` set to `NULL` ----
#' mw_estimate_prevalence_screening(
#'   df = anthro.02,
#'   muac = muac,
#'   edema = NULL,
#'   province
#' )
#'
#' ## Specifying the grouping variables ----
#' mw_estimate_prevalence_screening(
#'   df = anthro.02,
#'   muac = muac,
#'   edema = NULL,
#'   province
#' )
#'
#' @rdname muac-screening
#'
#' @export
#'
mw_estimate_prevalence_screening <- function(df,
                                             muac,
                                             edema = NULL,
                                             ...) {
  ## Difuse argument `.by` ----
  .by <- rlang::enquos(...)

  ## Empty vector type list to store results ----
  results <- list()

  ## Apply groupings if needed ----
  if (length(.by) > 0) df <- dplyr::group_by(df, !!!.by)
  
  ## Determine the analysis path that fits the data ----
    path <- dplyr::summarise(
      .data = df,
        age_ratio = rate_agesex_ratio(
          mw_stattest_ageratio(.data$age, .expectedP = 0.66)$p
        ),
        std = rate_std(
          stats::sd(
            remove_flags(as.numeric(.data$mfaz), "zscores"),
            na.rm = TRUE
          )
        ),
        analysis_approach = set_analysis_path(.data$age_ratio, .data$std),
        .groups = "keep"
      )

  ## Iterate over a data frame and compute estimates as per analysis path ----
  for (i in seq_len(nrow(path))) {
    if (length(.by) > 0) {
area <- dplyr::pull(path, !!.by[[1]])[i]
      data_subset <- dplyr::filter(df, !!.by[[1]] == area)
    } else {
      data_subset <- df
    }
 
    analysis_approach <- path$analysis_approach[i]
    if (analysis_approach == "unweighted") {
      if (length(.by) > 0) {
        output <- get_estimates(
          df = data_subset, 
          muac = {{ muac }}, 
          edema = {{ edema }}, 
          raw_muac = FALSE, 
          !!!.by
        )
      } else {
        output <- get_estimates(
          df = data_subset, 
          muac = {{ muac }}, 
          edema = {{ edema }},
          raw_muac = FALSE
        )
      }
    } else if (analysis_approach == "weighted") {
      if (length(.by) > 0) {
        output <- mw_estimate_smart_age_wt(
          df = data_subset, edema = {{ edema }}, raw_muac = FALSE, !!!.by
        )
      } else {
        output <- mw_estimate_smart_age_wt(
          df = data_subset, edema = {{ edema }}, raw_muac = FALSE
        )
      }
    } else {
      ## Return NA's ----
      if (length(.by) > 0) {
        output <- data_subset |> 
          dplyr::group_by(!!!.by) |> 
          dplyr::summarise(
          gam_p = NA_real_,
          sam_p = NA_real_,
          mam_p = NA_real_,
          .groups = "drop"
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
  if (length(.by) > 0) {
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


#'
#'
#'
#' @examples
#'
#' anthro.01 |>
#'   mw_wrangle_muac(
#'     sex = sex,
#'     .recode_sex = TRUE,
#'     muac = muac
#'   ) |>
#'   transform(
#'     age_cat = ifelse(age < 24, "6-23", "24-59")
#'   ) |>
#'   mw_estimate_prevalence_screening2(
#'     age_cat = age_cat,
#'     muac = muac,
#'     edema = edema,
#'     area
#'   )
#'
#' @rdname muac-screening
#'
#'
#' @export
#'
#'
mw_estimate_prevalence_screening2 <- function(
    df, age_cat, muac, edema = NULL, ...) {
  ## Difuse argument `.by` ----
  .by <- rlang::enquos(...)

  ## Empty vector of type list ----
  results <- list()

  ## Apply grouping if needed ----
  if (length(.by) > 0) df <- dplyr::group_by(df, !!!.by)

  ## Determine the analysis path that fits the data ----
    path <- df |>
      dplyr::summarise(
        age_ratio = rate_agesex_ratio(
          mw_stattest_ageratio2(
            age_cat, 0.66
          )$p
        ),
        std = rate_std(
          stats::sd(
            remove_flags(
              as.numeric(.data$muac),
              .from = "raw_muac"
            ),
            na.rm = TRUE
          ),
          .of = "raw_muac"
        ),
        analysis_approach = set_analysis_path(
          ageratio_class = .data$age_ratio,
          sd_class = .data$std
        ), 
        .groups = "keep"
      )
  
  ## Loop over groups ----
  for (i in seq_len(nrow(path))) {
  if (length(.by) > 0) {
    g <- dplyr::pull(path, !!.by[[1]])[i]
    data_subset <- dplyr::filter(df, !!.by[[1]] == g)
  } else {
    data_subset <- df
  }
    
  analysis_approach <- path$analysis_approach[i]

  if (analysis_approach == "unweighted") {
    if (length(.by) > 0) {
      r <- get_estimates(
        df = data_subset,
        muac = {{ muac }},
        edema = {{ edema }},
        raw_muac = TRUE,
        !!!.by
      )
    } else {
      r <- get_estimates(
        df = data_subset,
        muac = {{ muac }},
        edema = {{ edema }},
        raw_muac = TRUE
      )
    }
  } else if (analysis_approach == "weighted") {
    if (length(.by) > 0) {
     r <- mw_estimate_smart_age_wt(
        df = data_subset,
        edema = {{ edema }},
        raw_muac = TRUE,
        !!!.by
      )
    } else {
      r <- mw_estimate_smart_age_wt(
        df = data_subset,
        edema = {{ edema }},
        raw_muac = TRUE
      )
    }
  } else {
    if (length(.by) > 0) {
      r <- data_subset |> 
        dplyr::group_by(!!!.by) |> 
      dplyr::summarise(
        gam_p = NA_real_,
        sam_p = NA_real_,
        mam_p = NA_real_,
        .groups = "drop"
      )
    } else {
      ## Return NA's  ----
      r <- tibble::tibble(
        gam_p = NA_real_,
        sam_p = NA_real_,
        mam_p = NA_real_
      )
    }
  }
      results[[i]] <- r
  }

   ### Ensure that all categories in `.by` get added to the tibble ----
  if (length(.by) > 0) {
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