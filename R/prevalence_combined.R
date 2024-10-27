#'
#' Compute combined prevalence of wasting
#'
#' @rdname combined_prevalence
#'
compute_pps_based_combined_prevalence <- function(df,
                                              .wt = NULL,
                                              .edema = NULL,
                                              .summary_by) {
  ## Case definition ----
  df <- with(
    df,
    define_wasting(df,
      zscore = .data$wfhz,
      muac = .data$muac,
      edema = {{ .edema }},
      base = "combined"
    ) |>
      mutate(
        cflags = ifelse(.data$flag_wfhz == 1 | .data$flag_mfaz == 1, 1, 0)
      )
  )
  ## Create survey object ----
  if (!is.null(.wt)) {
    srvy <- df |>
      as_survey_design(
        ids = .data$cluster,
        pps = "brewer",
        variance = "YG",
        weights = {{ .wt }}
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
    group_by({{ .summary_by }}) |>
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
#' Compute the prevalence of combined wasting
#'
#' @description
#' The prevalence is calculated in accordance with the complex sample design
#' properties inherent to surveys. This includes weighting of survey data where
#' applicable. When either the acceptability of the standard deviation of WFHZ or
#' of the age ratio test is problematic, prevalence is not calculated.
#'
#' @param df An already wrangled dataset of class `data.frame` to use. Both
#' wranglers (of WFHZ and of MUAC) need to be used sequentially, regardless of the
#' order. Note that MUAC values should be converted to millimeters after using
#' the MUAC wrangler.
#'
#' @param .wt A vector of class `double` of the final survey weights. Default is
#'  `NULL` assuming a self-weighted survey, as in the ENA for SMART software;
#'  otherwise a weighted analysis is computed.
#'
#' @param .edema A vector of class `character` of edema. Code should be
#' "y" for presence and "n" for absence of bilateral edema. Default is `NULL`.
#'
#' @param .summary_by A vector of class `character` of the geographical areas
#' where the data was collected and for which the analysis should be performed.
#'
#' @returns A summarised table of class `data.frame` for the descriptive
#' statistics about combined wasting.
#'
#' @details
#' A concept of "combined flags" is introduced in this function. It consists of
#' defining as flag any observation that is flagged in either `flag_wfhz` or
#' `flag_mfaz` vectors. A new column `cflags` for combined flags is created and
#' added to `df`. This ensures that all flagged observations from both WFHZ
#' and MFAZ data are excluded from the combined prevalence analysis.
#'
#' *The table below shows an overview of how `cflags` are defined*
#' | **flag_wfhz** | **flag_mfaz** | **cflags** |
#' | :---: | :---: | :---: |
#' | 1 | 0  | 1 |
#' | 0 | 1  | 1 |
#' | 0 | 0  | 0 |
#'
#' @examples
#'
#' ## When .summary_by and .wt are set to NULL ----
#' p <- compute_combined_prevalence(
#' df = anthro.02,
#' .wt = NULL,
#' .edema = edema,
#' .summary_by = NULL
#' )
#'
#' print(p)
#'
#' ## When .wt is not set to NULL ----
#' x <- compute_combined_prevalence(
#' df = anthro.02,
#' .wt = "wtfactor",
#' .edema = edema,
#' .summary_by = NULL
#' )
#'
#' print(x)
#'
#' ## When working on data frame with multiple survey areas ----
#' s <- anthro.03 |>
#' mw_wrangle_age(
#' dos = NULL,
#' dob = NULL,
#' age = age,
#' .decimals = 2
#' ) |>
#' mw_wrangle_muac(
#' sex = sex,
#' muac = muac,
#' age = "age",
#' .recode_sex = TRUE,
#' .recode_muac = TRUE,
#' .to = "cm"
#' ) |>
#' dplyr::mutate(muac = recode_muac(muac, .to = "mm")) |>
#' mw_wrangle_wfhz(
#' sex = sex,
#' weight = weight,
#' height = height,
#' .recode_sex = TRUE) |>
#' compute_combined_prevalence(
#' .edema = edema,
#' .summary_by = district
#' )
#'
#' print(s)
#'
#' @export
#'
#' @rdname combined_prevalence
#'
compute_combined_prevalence <- function(df,
                                        .wt = NULL,
                                        .edema = NULL,
                                        .summary_by = NULL) {
  ## Difuse argument .summary_by ----
  .summary_by <- rlang::enquo(.summary_by)

  ## An empty vector type list ----
  results <- list()

  if (!rlang::quo_is_null(.summary_by)) {
    ## Grouped summary of standard deviation classification ----
    x <- summarise(
      df,
      std_wfhz = classify_sd(sd(remove_flags(as.numeric(.data$wfhz), "zscores"), na.rm = TRUE)),
      age_ratio = classify_age_sex_ratio(mw_stattest_ageratio(.data$age, .expectedP = 0.66)$p),
      std_mfaz = classify_sd(sd(remove_flags(as.numeric(.data$mfaz), "zscores"), na.rm = TRUE)),
      muac_analysis_approach = tell_muac_analysis_strategy(.data$age_ratio, .data$std_mfaz),
      .by = !!.summary_by
    )
  } else {
    ## Non-grouped summary ----
    x <- summarise(
      df,
      std_wfhz = classify_sd(sd(remove_flags(as.numeric(.data$wfhz), "zscores"), na.rm = TRUE)),
      age_ratio = classify_age_sex_ratio(mw_stattest_ageratio(.data$age, .expectedP = 0.66)$p),
      std_mfaz = classify_sd(sd(remove_flags(as.numeric(.data$mfaz), "zscores"), na.rm = TRUE)),
      muac_analysis_approach = tell_muac_analysis_strategy(.data$age_ratio, .data$std_mfaz),
    )
  }

  ## Iterate over data frame to compute prevalence according to the SD ----
  for (i in seq_len(nrow(x))) {
    if (!rlang::quo_is_null(.summary_by)) {
      area <- dplyr::pull(x, !!.summary_by)[i]
      data <- filter(df, !!sym(rlang::quo_name(.summary_by)) == !!area)
    } else {
      data <- df
    }

    std_wfhz <- x$std_wfhz[i]
    muac_analysis_approach <- x$muac_analysis_approach[i]

    if (std_wfhz != "Problematic" && muac_analysis_approach == "unweighted") {
      ### Compute standard complex sample based prevalence analysis ----
      output <- compute_pps_based_combined_prevalence(
        data,
        .wt = {{ .wt }},
        .edema = {{ .edema }},
        .summary_by = !!.summary_by
      )
    } else {
      ## Add grouped NA's ----
      if (!rlang::quo_is_null(.summary_by)) {
        output <- summarise(
          data,
          cgam_p = NA_real_,
          csam_p = NA_real_,
          cmam_p = NA_real_,
          .by = !!.summary_by
        )
      } else {
        ## Add non-grouped NA's ----
        output <- tibble::tibble(
          cgam_p = NA_real_,
          csam_p = NA_real_,
          cmam_p = NA_real_
        )
      }
    }
    results[[i]] <- output
  }
  ### Ensure that all geographical areas are added to the tibble ----
  if (!rlang::quo_is_null(.summary_by)) {
    results <- dplyr::bind_rows(results) |>
      dplyr::relocate(.data$cgam_p, .after = .data$cgam_n) |>
      dplyr::relocate(.data$csam_p, .after = .data$csam_n) |>
      dplyr::relocate(.data$cmam_p, .after = .data$cmam_n)
  } else {
    ## Non-grouped results
    results <- dplyr::bind_rows(results)
  }
  results
}
