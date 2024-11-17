#'
#'
#' @keywords internal
#'
#'
get_estimates <- function(df, muac, edema = NULL, .by = NULL) {
  muac <- eval_tidy(enquo(muac), df)
  edema <- eval_tidy(enquo(edema), df)

  ## Enforce class of `muac` ----
  if (!is.numeric(muac)) {
    stop("`muac` should be of class numeric; not ", shQuote(class(muac)), ". Try again!")
  }

  ### Enforce measuring unit is in "mm" ----
  if (any(grepl("\\.", as.character(pull(df, .data$muac))))) {
    stop("MUAC values must be in millimeters. Try again!")
  }


  ## Wasting definition including `edema` ----
  if (!is.null(edema)) {
    ### Enforce class of `edema` ----
    if (!is.character(edema)) {
      stop("`edema` should be of class character; not ", shQuote(class(edema)), ". Try again!")
    }
    ### Enforce code values in `edema` ----
    if (!all(levels(as.factor(edema)) %in% c("y", "n"))) {
      stop("Code values in `edema` must only be 'y' and 'n'. Try again!")
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
  ## Summarize results ----
  p <- x |>
    group_by({{ .by }}) |>
    filter(.data$flag_mfaz == 0) |>
    summarise(
      across(
        c(.data$gam:.data$mam),
        list(
          n = \(.)sum(., na.rm = TRUE),
          p = \(.)mean(., na.rm = TRUE)
        )
      )
    )
  p
}


#'
#'
#' Estimate the prevalence of wasting based on MUAC for non survey data
#'
#' @description
#' It is common to estimate prevalence of wasting from non survey data, such
#' as screenings or any other community-based surveillance systems. In such
#' situations, the analysis usually consists only in estimating the point prevalence
#' and the counts of positive cases, without necessarily estimating the
#' uncertainty. This is the job of this function.
#'
#' Before estimating, it evaluates the quality of data by calculating and rating the
#' standard deviation of z-scores of muac-for-age (MFAZ) and the p-value of the
#' age ratio test; then it sets the analysis path that best fits the data.
#'
#'  + If all tests are rated as not problematic, a normal analysis is done.
#'  + If standard deviation is not problematic and age ratio test is problematic,
#'  prevalence is age-weighted. This is to fix the likely overestimation of wasting
#'   when there are excess of younger children in the data set.
#'  + If standard deviation is problematic and age ratio test is not, or both
#'  are problematic,  analysis gets cancelled out and `NA`s get thrown.
#'
#' Outliers are detected based on SMART flags on the MFAZ values and then
#' get excluded prior being piped into the actual prevalence analysis workflow.
#'
#' @param df A data set object of class `data.frame` to use. This must have been
#' wrangled using this package's wrangling function for MUAC data. Make sure
#' MUAC values are converted to millimeters after using the wrangler.
#' If this is not done, the function will stop execution and return an error message
#' with the issue.
#'
#' @param muac A vector of raw MUAC values of class `numeric` or `integer`.
#' The measurement unit of the values should be millimeters. If any or all values
#' are in a different unit than the expected, the function will stop execution and
#' return an error message indicating the issue.
#'
#' @param edema A vector of class `character` of edema. Code should be
#' "y" for presence and "n" for absence of bilateral edema. Default is `NULL`.
#' If class, as well as, code values are different than expected, the function
#' will stop the execution and return an error message indicating the issue.
#'
#' @param .by A vector of class `character` or `numeric` of the geographical areas
#' or respective IDs for where the data was collected and for which the analysis
#' should be summarized at.
#'
#' @returns A summarized table of class `data.frame` of the descriptive
#' statistics about wasting.
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
    path <- df |>
      group_by(!!.by) |>
      summarise(
        age_ratio = rate_agesex_ratio(mw_stattest_ageratio(.data$age, .expectedP = 0.66)$p),
        std = rate_std(sd(remove_flags(as.numeric(.data$mfaz), "zscores"), na.rm = TRUE)),
        analysis_approach = set_analysis_path(.data$age_ratio, .data$std),
        .groups = "drop"
      )
  } else {
    path <- df |>
      summarise(
        age_ratio = rate_agesex_ratio(mw_stattest_ageratio(.data$age, .expectedP = 0.66)$p),
        std = rate_std(sd(remove_flags(as.numeric(.data$mfaz), "zscores"), na.rm = TRUE)),
        analysis_approach = set_analysis_path(.data$age_ratio, .data$std)
      )
  }

  ## Iterate over a data frame and compute estimates as per analysis path ----
  for (i in seq_len(nrow(path))) {
    if (!quo_is_null(.by)) {
      area <- pull(path, !!.by)[i]
      data <- filter(df, !!sym(quo_name(.by)) == area)
    } else {
      data <- df
    }

    analysis_approach <- path$analysis_approach[i]
    if (analysis_approach == "unweighted") {
      if (!quo_is_null(.by)) {
        output <- get_estimates(df = data, muac = {{ muac }}, edema = {{ edema }}, .by = !!.by)
      } else {
        output <- get_estimates(df = data, muac = {{ muac }}, edema = {{ edema }})
      }
    } else if (analysis_approach == "weighted") {
      if (!quo_is_null(.by)) {
        output <- mw_estimate_smart_age_wt(df = data, edema = {{ edema }}, .by = !!.by)
      } else {
        output <- mw_estimate_smart_age_wt(df = data, edema = {{ edema }})
      }
    } else {
      ## Return NA's ----
      if (!quo_is_null(.by)) {
        output <- summarise(
          data,
          gam_p = NA_real_,
          sam_p = NA_real_,
          mam_p = NA_real_,
          .by = !!.by
        )
      } else {
        ## Return NA's  ----
        output <- tibble(
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
    results <- bind_rows(results) |>
      relocate(.data$gam_p, .after = .data$gam_n) |>
      relocate(.data$sam_p, .after = .data$sam_n) |>
      relocate(.data$mam_p, .after = .data$mam_n)
  } else {
    ## Non-grouped results
    results <- bind_rows(results)
  }
  results
}
