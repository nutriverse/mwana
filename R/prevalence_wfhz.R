#'
#'
#'
#'
get_complex_sample_estimates <- function(df,
                                         wt = NULL,
                                         edema = NULL,
                                         .by) {
  ## Define wasting and add to the data frame ----
  df <- with(
    df,
    define_wasting(
      df,
      zscores = .data$wfhz,
      edema = {{ edema }},
      .by = "zscores"
    )
  )

  ## Create a survey object ----
  if (!is.null(wt)) {
    srvy <- df |>
      as_survey_design(
        ids = .data$cluster,
        pps = "brewer",
        variance = "YG",
        weights = {{ wt }}
      )
  } else {
    ## Create a vector filled of 1 for self-weights ----
    df$wt <- rep(1, length(df$cluster))
    ## Create survey object ----
    srvy <- df |>
      as_survey_design(
        ids = .data$cluster,
        pps = "brewer",
        variance = "YG",
        weights = wt
      )
  }

  ## Summarise prevalence ----
  p <- srvy |>
    group_by({{ .by }}) |>
    filter(.data$flag_wfhz == 0) |>
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
#' Compute the prevalence estimates of wasting on the basis of WFHZ, MFAZ or MUAC
#'
#' @description
#' The prevalence is calculated in accordance with the complex sample design
#' properties inherent to surveys. This includes weighting the survey data where
#' applicable and applying PROBIT method estimation (for WFHZ) when the standard
#' deviation is problematic. This is as in the SMART Methodology.
#'
#' @param df An already wrangled dataset object of class `data.frame` to use.
#'
#' @param wt A vector of class `double` of the final survey weights. Default is
#'  `NULL` assuming a self weighted survey, as in the ENA for SMART software;
#'  otherwise, when a vector of weights if supplied, weighted analysis is computed.
#'
#' @param edema A vector of class `character` of edema. Code should be
#' "y" for presence and "n" for absence of bilateral edema. Default is `NULL`.
#'
#' @param .by A vector of class `character` of the geographical areas
#' where the data was collected and for which the analysis should be performed.
#'
#' @returns A summarised table of class `data.frame` of the descriptive
#' statistics about wasting.
#'
#' @examples
#' ## An example of application of `compute_wfhz_prevalence()` ----
#'
#' ### When .by = NULL ----
#' anthro.03 |>
#'   mw_wrangle_wfhz(
#'     sex = sex,
#'     weight = weight,
#'     height = height,
#'     .recode_sex = TRUE
#'   ) |>
#'   mw_estimate_wfhz_prevalence(
#'     wt = NULL,
#'     edema = edema,
#'     .by = NULL
#'   )
#'
#' ### When .by is not set to NULL ----
#'
#' anthro.03 |>
#'   mw_wrangle_wfhz(
#'     sex = sex,
#'     weight = weight,
#'     height = height,
#'     .recode_sex = TRUE
#'   ) |>
#'   mw_estimate_wfhz_prevalence(
#'     wt = NULL,
#'     edema = edema,
#'     .by = district
#'   )
#'
#' ### When a weighted analysis is needed ----
#'
#' anthro.02 |>
#'   mw_estimate_wfhz_prevalence(
#'     wt = "wtfactor",
#'     edema = edema,
#'     .by = province
#'   )
#'
#' @rdname prevalence
#'
#' @export
#'
mw_estimate_wfhz_prevalence <- function(df,
                                        wt = NULL,
                                        edema = NULL,
                                        .by = NULL) {
  ## Difuse argument .by ----
  .by <- enquo(.by)

  ## An empty vector type list ----
  results <- list()

  if (!rlang::quo_is_null(.by)) {
    ## Grouped summary of standard deviation classification ----
    x <- summarise(
      df,
      std = rate_std(sd(remove_flags(.data$wfhz, "zscores"), na.rm = TRUE)),
      .by = !!.by
    )
  } else {
    ## Non-grouped summary ----
    x <- summarise(
      df,
      std = rate_std(sd(remove_flags(.data$wfhz, "zscores"), na.rm = TRUE))
    )
  }

  ## Iterate over data frame to compute prevalence according to the SD ----
  for (i in seq_len(nrow(x))) {
    if (!quo_is_null(.by)) {
      area <- dplyr::pull(x, !!.by)[i]
      data <- filter(df, !!sym(quo_name(.by)) == !!area)
    } else {
      data <- df
    }

    std <- x$std[i]
    if (std != "Problematic") {
      ### Compute standard complex sample based prevalence analysis ----
      result <- get_complex_sample_estimates(data, {{ wt }}, {{ edema }}, !!.by)
    } else {
      ### Compute grouped PROBIT based prevalence ----
      if (!quo_is_null(.by)) {
        result <- estimate_probit_prevalence(data, !!.by, .for = "wfhz")
      } else {
        ### Compute non-grouped PROBIT based prevalence ----
        result <- estimate_probit_prevalence(data, .for = "wfhz")
      }
    }
    results[[i]] <- result
  }
  bind_rows(results) |>
    relocate(.data$gam_p, .after = .data$gam_n) |>
    relocate(.data$sam_p, .after = .data$sam_n) |>
    relocate(.data$mam_p, .after = .data$mam_n)
}
