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
  case_when(
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
smart_age_weighted_estimate <- function(df, edema = NULL, .by = NULL) {
  ## Difuse argument `.by` ----
  .by <- enquo(.by)

  if (!quo_is_null(.by)) {
    df <- df |>
      filter(.data$flag_mfaz == 0) |>
      summarise(
        sam = smart_age_weighting(.data$muac, .data$age, {{ edema }}, .form = "sam"),
        mam = smart_age_weighting(.data$muac, .data$age, {{ edema }}, .form = "mam"),
        gam = sum(.data$sam, .data$mam),
        .by = !!.by
      ) |>
      rename(
        gam_p = .data$gam,
        sam_p = .data$sam,
        mam_p = .data$mam
      )
  } else {
    df <- df |>
      filter(.data$flag_mfaz == 0) |>
      summarise(
        sam = smart_age_weighting(.data$muac, .data$age, {{ edema }}, .form = "sam"),
        mam = smart_age_weighting(.data$muac, .data$age, {{ edema }}, .form = "mam"),
        gam = sum(.data$sam, .data$mam)
      ) |>
      rename(
        gam_p = .data$gam,
        sam_p = .data$sam,
        mam_p = .data$mam
      )
  }
  df
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
  ## Difuse ----
  wt <- enquo(wt)

  df <- df |>
    define_wasting(
      muac = .data$muac,
      edema = {{ edema }},
      .by = "muac"
    )

  ### Weighted survey analysis ----
  if (!is.null(wt)) {
    srvy <- df |>
      as_survey_design(
        ids = .data$cluster,
        pps = "brewer",
        variance = "YG",
        weights = !!wt
      )
  } else {
    ### Unweighted: typical SMART survey analysis ----
    srvy <- df |>
      as_survey_design(
        ids = .data$cluster,
        pps = "brewer",
        variance = "YG"
      )
  }
  #### Summarise prevalence ----
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
      wt_pop = sum(srvyr::cur_svy_wts())
    )
  p
}


#'
#' This is to be revised
#'
#' @examples
#'
#' ## An example of application of `mw_estimate_prev_wasting_muac()` ----
#'
#' ### When .summary.by = NULL ----
#'
#' prev <- mw_estimate_prev_wasting_muac(
#'   df = anthro.04,
#'   wt = NULL,
#'   edema = edema,
#'   .by = NULL
#' )
#'
#' print(prev)
#'
#' ### When .by is not set to NULL ----
#'
#' p <- mw_estimate_prev_wasting_muac(
#'   df = anthro.04,
#'   wt = NULL,
#'   edema = edema,
#'   .by = province
#' )
#'
#' print(p)
#'
#' @export
#'
mw_estimate_prev_wasting_muac <- function(df,
                                          wt = NULL,
                                          edema = NULL,
                                          .by = NULL) {
  ## Difuse argument `.by` ----
  .by <- enquo(.by)

  ## Empty vector type list to store results ----
  results <- list()

  if (!quo_is_null(.by)) {
    ## Evaluate the analysis path by `.by`  ----
    x <- df |>
      group_by(!!.by) |>
      summarise(
        age_ratio = rate_agesex_ratio(mw_stattest_ageratio(.data$age, .expectedP = 0.66)$p),
        std = rate_std(sd(remove_flags(as.numeric(.data$mfaz), "zscores"), na.rm = TRUE)),
        analysis_approach = set_analysis_path(.data$age_ratio, .data$std),
        .groups = "drop"
      )
  } else {
    ## Evaluate the analysis path ----
    x <- df |>
      summarise(
        age_ratio = rate_agesex_ratio(mw_stattest_ageratio(.data$age, .expectedP = 0.66)$p),
        std = rate_std(sd(remove_flags(as.numeric(.data$mfaz), "zscores"), na.rm = TRUE)),
        analysis_approach = set_analysis_path(.data$age_ratio, .data$std)
      )
  }

  ## Iterate over a data frame and compute estimates as per analysis path ----
  for (i in seq_len(nrow(x))) {
    if (!quo_is_null(.by)) {
      area <- pull(x, !!.by)[i]
      data <- filter(df, !!sym(quo_name(.by)) == area)
    } else {
      data <- df
    }

    analysis_approach <- x$analysis_approach[i]

    if (analysis_approach == "unweighted") {
      ### Estimate PPS-based prevalence ----
      output <- complex_survey_estimates_muac(data, {{ wt }}, {{ edema }}, !!.by)
    } else if (analysis_approach == "weighted") {
      ### Estimate age-weighted prevalence as per SMART MUAC tool ----
      if (!quo_is_null(.by)) {
        output <- smart_age_weighted_estimate(data, edema = {{ edema }}, !!.by)
      } else {
        ### Estimate age-weighted prevalence as per SMART MUAC tool ----
        output <- smart_age_weighted_estimate(data, edema = {{ edema }})
      }
    } else {
      ## Return NA's ----
      if (!quo_is_null(.by)) {
        output <- data |>
          summarise(
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
