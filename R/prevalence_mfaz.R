#'
#'
#'
compute_pps_based_mfaz_prevalence <- function(df,
                                              .wt = NULL,
                                              .edema = NULL,
                                              .summary_by) {

  ## Add acute malnutrition case-definitions to the data frame ----
  df <- with(
    df,
    define_wasting(
      df,
      zscore = .data$mfaz,
      edema = {{ .edema }},
      base = "wfhz"
    )
  )
  ## Create a survey object ----
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
#'
#' @rdname prevalence
#'
#'
compute_mfaz_prevalence <- function(df,
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
      std = classify_sd(sd(remove_flags(.data$mfaz, "zscores"), na.rm = TRUE)),
      .by = !!.summary_by
    )
  } else {
    ## Non-grouped summary ----
    x <- summarise(
      df,
      std = classify_sd(sd(remove_flags(.data$mfaz, "zscores"), na.rm = TRUE))
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

    std <- x$std[i]
    if (std != "Problematic") {
      ### Compute standard complex sample based prevalence analysis ----
      result <- compute_pps_based_mfaz_prevalence(data, {{ .wt }}, {{ .edema }}, !!.summary_by)
    } else {
      ### Compute grouped PROBIT based prevalence ----
      if (!rlang::quo_is_null(.summary_by)) {
        result <- compute_probit_prevalence(data, !!.summary_by, .for = "mfaz")
      } else {
        ### Compute non-grouped PROBIT based prevalence ----
        result <- compute_probit_prevalence(data, .for = "mfaz")
      }
    }
    results[[i]] <- result
  }
  dplyr::bind_rows(results) |>
    dplyr::relocate(.data$gam_p, .after = .data$gam_n) |>
    dplyr::relocate(.data$sam_p, .after = .data$sam_n) |>
    dplyr::relocate(.data$mam_p, .after = .data$mam_n)
}

