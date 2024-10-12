#'
#' Compute the prevalence of wasting on the basis of WFHZ, MFAZ and MUAC
#'
#' @description
#' The prevalence is calculated in accordance with the complex sample design
#' properties inherent to surveys. This includes weighting the survey data where
#' applicable and applying PROBIT method estimation of prevalence (for WFHZ and
#' MFAZ) when standard deviation is problematic. This is as in the SMART Methodology.
#'
#' @param df A data frame object with the required variables already wrangled.
#'
#' @param .wt A numeric vector holding final survey weights. When set to `NULL`,
#' the function assumes self weighted survey, as in the ENA for SMART software;
#' Otherwise when supplied, weighted analysis is computed.
#'
#' @param .edema A character vector indicating if an observation has bilateral
#' edema or not. The codes are "y" for presence and "n" for absence of bilateral
#' edema. Default is `NULL`.
#'
#' @param .summary_by A character vector containing data on the geographical areas
#' where the data was collected and for which the analysis should be performed at.
#'
#' @returns A table with the descriptive statistics about wasting.
#'
#' @examples
#' ## An example of application of `compute_wfhz_prevalence()` ----
#'
#' ### When .summary_by = NULL ----
#' anthro.03 |>
#' process_wfhz_data(
#' sex = sex,
#' weight = weight,
#' height = height,
#' .recode_sex = TRUE
#' ) |>
#' compute_wfhz_prevalence(
#' .wt = NULL,
#' .edema = edema,
#' .summary_by = NULL
#' )
#'
#' ### When .summary_by is not set to NULL ----
#'
#' anthro.03 |>
#' process_wfhz_data(
#' sex = sex,
#' weight = weight,
#' height = height,
#' .recode_sex = TRUE
#' ) |>
#' compute_wfhz_prevalence(
#' .wt = NULL,
#' .edema = edema,
#' .summary_by = district
#' )
#'
#' ### When a weighted analysis is needed ----
#'
#' anthro.02 |>
#' compute_wfhz_prevalence(
#' .wt = "wtfactor",
#' .edema = edema,
#' .summary_by = province
#' )
#'
#' @rdname prevalence
#'
#' @export
#'
compute_wfhz_prevalence <- function(df,
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
      std = classify_sd(sd(remove_flags(.data$wfhz, "zscore"), na.rm = TRUE)),
      .by = !!.summary_by
    )
  } else {
    ## Non-grouped summary ----
    x <- summarise(
      df,
      std = classify_sd(sd(remove_flags(.data$wfhz, "zscore"), na.rm = TRUE))
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
      result <- compute_pps_based_wfhz_prevalence(data, {{ .wt }}, {{ .edema }}, !!.summary_by)
    } else {
      ### Compute grouped PROBIT based prevalence ----
      if (!rlang::quo_is_null(.summary_by)) {
        result <- compute_probit_prevalence(data, !!.summary_by, .for = "wfhz")
      } else {
        ### Compute non-grouped PROBIT based prevalence ----
        result <- compute_probit_prevalence(data, .for = "wfhz")
      }
    }
    results[[i]] <- result
  }
  dplyr::bind_rows(results) |>
    dplyr::relocate(.data$gam_p, .after = .data$gam_n) |>
    dplyr::relocate(.data$sam_p, .after = .data$sam_n) |>
    dplyr::relocate(.data$mam_p, .after = .data$mam_n)
}



#'
#'
#'
#'
compute_pps_based_wfhz_prevalence <- function(df,
                                              .wt = NULL,
                                              .edema = NULL,
                                              .summary_by) {

  ## Add acute malnutrition case-definitions to the data frame ----
  df <- with(
    df,
    define_wasting(
      df,
      zscore = .data$wfhz,
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
#'
#' Compute the prevalence of wasting on the basis of the PROBIT method.
#'
#' @description
#' This approach is applied when the standard deviation of WFHZ is problematic.
#' The PROBIT method estimates the prevalence of wasting indirectly by calculating
#' the area under the curve the tail of the curve, from negative infinitive to
#' the given threshold, using the cumulative normal distribution function with
#' the mean and standard deviation as inputs.
#'
#' @param df A data frame object with the required variables already wrangled.
#'
#' @param x A numeric vector holding WFHZ or MFAZ values.
#'
#' @param .status A choice on the form of wasting for which the prevalence should
#' be calculated for.
#'
#' @param .summary_by A character vector containing data on the geographical areas where
#'  the data was collected. This is to group the survey design object into different
#'  geographical areas in the data and allow for summaries to be computed for each of them.
#'  Default is NULL.
#'
#' @param .for A choice between "wfhz" and "mfaz" for the anthropometric index you wish
#'  to compute PROBIT prevalence on.
#'
#' @returns A data frame with the prevalence. No confidence intervals are
#' yielded.
#'
#' @rdname probit-method
#'
#'
apply_probit_approach <- function(x, .status = c("gam", "sam")) {
  .status <- match.arg(.status)
  mean <- mean(remove_flags(x, "zscore"), na.rm = TRUE)
  ## Return GAM and SAM prevalence with a SD = 1
  switch(
    .status,
    "gam" = {pnorm(q = -2, mean = mean, sd = 1, lower.tail = TRUE, log.p = FALSE)},
    "sam" = {pnorm(q = -3, mean = mean, sd = 1, lower.tail = TRUE, log.p = FALSE)}
  )
}



#'
#'
#' @rdname probit-method
#'
compute_probit_prevalence <- function(df,
                                      .summary_by = NULL,
                                      .for = c("wfhz", "mfaz")) {
  ## Difuse argument ----
  .summary_by <- rlang::enquo(.summary_by)
  ## Match argument ----
  .for <- match.arg(.for)

  switch(
    .for,
    "wfhz" = {
      if(!is.null(.summary_by)) {
        df <- summarise(
          df,
          gam = apply_probit_approach(.data$wfhz, .status = "gam"),
          sam = apply_probit_approach(.data$wfhz, .status = "sam"),
          mam = .data$gam - .data$sam,
          .by = !!.summary_by
        ) |>
          mutate(
            gam_p = .data$gam, sam_p = .data$sam, mam_p = .data$mam,
            gam = NA, sam = NA, mam = NA
          ) |>
          dplyr::select(!2:4) ## To make it fit in the tibble structure from the main function
      } else {
        df <- summarise(
          df,
          gam = apply_probit_approach(.data$wfhz, .status = "gam"),
          sam = apply_probit_approach(.data$wfhz, .status = "sam"),
          mam = .data$gam - .data$sam
        ) |>
          mutate(
            gam_p = .data$gam, sam_p = .data$sam, mam_p = .data$mam,
            gam = NA, sam = NA, mam = NA
          ) |>
          dplyr::select(!2:4) ## To make it fit in the tibble structure from the main function
      }
      df
    },
    "mfaz" = {
      if(!is.null(.summary_by)) {
        df <- summarise(
          df,
          gam = apply_probit_approach(.data$mfaz, .status = "gam"),
          sam = apply_probit_approach(.data$mfaz, .status = "sam"),
          mam = .data$gam - .data$sam,
          .by = !!.summary_by
        ) |>
          mutate(
            gam_p = .data$gam, sam_p = .data$sam, mam_p = .data$mam,
            gam = NA, sam = NA, mam = NA
          ) |>
          dplyr::select(!2:4) ## To make it fit in the tibble structure from the main function
      } else {
        df <- summarise(
          df,
          gam = apply_probit_approach(.data$mfaz, .status = "gam"),
          sam = apply_probit_approach(.data$mfaz, .status = "sam"),
          mam = .data$gam - .data$sam
        ) |>
          mutate(
            gam_p = .data$gam, sam_p = .data$sam, mam_p = .data$mam,
            gam = NA, sam = NA, mam = NA
          ) |>
          dplyr::select(!2:4) ## To make it fit in the tibble structure from the main function
      }
      df
    }
  )
}
