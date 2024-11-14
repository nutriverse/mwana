#'
#'
#' @keywords internal
#'
#'
complex_survey_estimates_combined <- function(df,
                                              wt = NULL,
                                              edema = NULL,
                                              .by) {
  ## Difuse arguments ----
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
        mutate(
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
        mutate(
          cflags = ifelse(.data$flag_wfhz == 1 | .data$flag_mfaz == 1, 1, 0)
        )
    )
  }

  ## Create survey object ----
  if (!quo_is_null(wt)) {
    srvy <- df |>
      as_survey_design(
        ids = .data$cluster,
        pps = "brewer",
        variance = "YG",
        weights = !!wt
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
    filter(.data$cflags == 0) |>
    summarise(
      across(
        c(.data$cgam:.data$cmam),
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
      wt_pop = sum(srvyr::cur_svy_wts())
    )
  p
}



#'
#'
#' Estimate the prevalence of combined wasting
#'
#' @description
#' Estimate the prevalence of wasting based on the combined case-definition of
#' weight-for-height z-scores (WFHZ), MUAC and/or edema. The function allows users to
#' get the prevalence estimates calculated in accordance with the complex sample
#' design properties; this includes applying survey weights when needed or applicable.
#' Flagged records in WFHZ and in MUAC data set are excluded prior prevalence analysis.
#' Alongside this, the function also checks for the quality of
#' data set before computing the prevalence. If all standard deviations of
#' WFHZ and MFAZ, as well the age ratio test are not problematic, analysis is done;
#' otherwise, if any of the checks get rated as problematic, NAs get thrown.
#'
#' @param df A dataset object of class `data.frame` to use. This must have been
#' wrangled using this package's wrangling functions for both WFHZ and MUAC data
#' sequentially. The order does not matter. Note that MUAC values should be
#' converted to millimeters after using the MUAC wrangler. If this is not done,
#' the function will stop execution and return an error message. Moreover, the
#' function uses a variable called `cluster` where the primary sampling unit IDs
#' are stored. Make sure to rename your cluster ID variable to `cluster`, otherwise
#' the function will error and terminate the execution.
#'
#' @param wt A vector of class `double` of the final survey weights. Default is
#'  `NULL` assuming a self-weighted survey, as in the ENA for SMART software;
#'  otherwise a weighted analysis is computed.
#'
#' @param edema A vector of class `character` of edema. Code will be
#' "y" for presence and "n" for absence of bilateral edema. Default is `NULL`.
#'
#' @param .by A vector of class `character` or `numeric` of the geographical areas
#' or respective IDs for where the data was collected and for which the analysis
#' should be summarised at.
#'
#' @returns A summarised table of class `data.frame` for the descriptive
#' statistics about combined wasting.
#'
#' @details
#' A concept of "combined flags" is introduced in this function. It consists of
#' defining as flag any observation that is flagged in either `flag_wfhz` or
#' `flag_mfaz` vectors. A new column `cflags` for combined flags is created and
#' added to `df`. This ensures that all flagged observations from both WFHZ
#' and MFAZ data are excluded from the prevalence analysis.
#'
#' *A glimpse on how `cflags` are defined*
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
  ## Difuse argument `.by` ----
  .by <- enquo(.by)

  ## Enforce measuring unit is in "mm" ----
  x <- as.character(pull(df, muac))
  if (any(grepl("\\.", x))) {
    stop("MUAC values must be in millimeters. Please try again.")
  }

  ## Empty vector to store results ----
  results <- list()

  if (!quo_is_null(.by)) {
    ## Rate standard deviation and set MUAC analysis path ----
    x <- df |>
      summarise(
        std_wfhz = rate_std(sd(remove_flags(as.numeric(.data$wfhz), "zscores"), na.rm = TRUE)),
        age_ratio = rate_agesex_ratio(mw_stattest_ageratio(.data$age, .expectedP = 0.66)$p),
        std_mfaz = rate_std(sd(remove_flags(as.numeric(.data$mfaz), "zscores"), na.rm = TRUE)),
        muac_analysis_path = set_analysis_path(.data$age_ratio, .data$std_mfaz),
        .by = !!.by
      )
  } else {
    ## Rate standard deviation and set MUAC analysis path ----
    x <- df |>
      summarise(
        std_wfhz = rate_std(sd(remove_flags(as.numeric(.data$wfhz), "zscores"), na.rm = TRUE)),
        age_ratio = rate_agesex_ratio(mw_stattest_ageratio(.data$age, .expectedP = 0.66)$p),
        std_mfaz = rate_std(sd(remove_flags(as.numeric(.data$mfaz), "zscores"), na.rm = TRUE)),
        muac_analysis_path = set_analysis_path(.data$age_ratio, .data$std_mfaz)
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

    std_wfhz <- x$std_wfhz[i]
    muac_analysis_path <- x$muac_analysis_path[i]

    if (std_wfhz != "Problematic" && muac_analysis_path == "unweighted") {
      ### Compute standard complex sample based prevalence analysis ----
      output <- data |>
        complex_survey_estimates_combined(
          wt = {{ wt }},
          edema = {{ edema }},
          .by = !!.by
        )
    } else {
      ## Add NA ----
      if (!quo_is_null(.by)) {
        output <- data |>
          summarise(
            cgam_p = NA_real_,
            csam_p = NA_real_,
            cmam_p = NA_real_,
            .by = !!.by
          )
      } else {
        ## Add NA ----
        output <- tibble(
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
    results <- bind_rows(results) |>
      relocate(.data$cgam_p, .after = .data$cgam_n) |>
      relocate(.data$csam_p, .after = .data$csam_n) |>
      relocate(.data$cmam_p, .after = .data$cmam_n)
  } else {
    ## Ungrouped results
    results <- bind_rows(results)
  }
  results
}
