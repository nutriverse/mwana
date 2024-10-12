#'
#' A helper function to identify the MUAC prevalence analysis approach on the
#' basis of age ratio and standard deviation test results
#'
#' @description
#' This is a helper function that gives instruction, to the main MUAC prevalence
#' analysis function, on the analysis approach to follow in a given area of
#' analysis on the basis of the quality of the age ratio test and the standard
#' deviation.
#'
#' @param age_ratio_class A character vector returned from the plausibility
#' auditors holding the rating of the age ratio test results.
#'
#' @param sd_class A character vector returned from the plausibility auditors
#' holding the rating of the standard deviation test results.
#'
#' @returns A character vector of the same length as the input holding analysis
#' approach to be followed in a given area of analysis: "weighted", "unweighted" and
#' "missing". When "weighted", the CDC weighting approach is applied to correct for
#' age bias; "unweighted" a normal complex sample analysis is applied; when
#' "missing" `NA` gets thrown, so no prevalence computed.
#'
#'
tell_muac_analysis_strategy <- function(age_ratio_class, sd_class) {
  case_when(
    age_ratio_class == "Problematic" & sd_class != "Problematic" ~ "weighted",
    age_ratio_class != "Problematic" & sd_class == "Problematic" ~ "missing",
    age_ratio_class == "Problematic" & sd_class == "Problematic" ~ "missing",
    .default = "unweighted"
  )
}


#'
#'
#' Apply weighting to the MUAC prevalence when sample distribution is unbalanced
#' between children aged 6 to 23 months and those aged 24 to 59 months old
#'
#' @description
#' `apply_cdc_age_weighting()` calculates a weighted proportion by adding the
#' proportion of children under 2 years to twice the proportion of children over 2
#' and then dividing by 3.
#'
#' @param muac A numeric vector holding MUAC values (in mm).
#'
#' @param age A numeric vector holding child's age in months.
#'
#' @param .edema Optional. If given, it should be a character vector of "y"
#' for presence and "n" for absence of bilateral edema.
#'
#' @param status A choice between "sam" and "mam" for the form of wasting.
#'
#' @returns A numeric vector of length and size 1.
#'
#' @details
#' This function is informed by the output of [age_ratio_test()].
#' Note that this method differs from the approach used in the SMART plausibility
#' check. Please refer to the documentation for further details.
#'
#'
apply_cdc_age_weighting <- function(muac, age,
                                    .edema = NULL, status = c("sam", "mam")) {

  ## Match arguments ----
  status <- match.arg(status)

  if (!is.null(.edema)) {
    ### Define cases ----
    nut_status <- classify_wasting_for_cdc_approach(muac = muac, .edema = {{ .edema }})

    ### Compute age weighted prevalence ----
    age_group <- ifelse(age < 24, "under_2", "over_2")
    nut_U2 <- ifelse(age_group == "under_2" & nut_status == status, 1, 0)
    nut_O2 <- ifelse(age_group == "over_2" & nut_status == status, 1, 0)
    p <- mean(nut_U2, na.rm = TRUE) + (2 * mean(nut_O2, na.rm = TRUE)) / 3

  } else {
    ### Define cases ----
    nut_status <- classify_wasting_for_cdc_approach(muac)

    ### Compute age weighted prevalence ----
    age_group <- ifelse(age < 24, "under_2", "over_2")
    nut_U2 <- ifelse(age_group == "under_2" & nut_status == status, 1, 0)
    nut_O2 <- ifelse(age_group == "over_2" & nut_status == status, 1, 0)
    p <- mean(nut_U2, na.rm = TRUE) + (2 * mean(nut_O2, na.rm = TRUE)) / 3
  }
  p
}



#'
#'
#' Apply weighting to the MUAC prevalence when sample distribution is unbalanced
#' between children aged 6 to 23 months and those aged 24 to 59 months old
#'
#' @param df A data frame object with the required variables already wrangled.
#'
#' @param .edema A character vector indicating if an observation has bilateral
#' edema or not. The codes are "y" for presence and "n" for absence of bilateral
#' edema. Default is `NULL`.
#'
#' @param .summary_by A character vector containing data of the geographical areas
#' where the data was collected and for which the analysis should be performed at.
#'
#' @returns A tibble with dimensions that vary based on the use of `.summary_by`.
#' If set to `NULL`, a 1 x 3 tibble is returned. Otherwise, the number of rows
#' will match the number of groups or areas provided in `.summary_by`,
#' while the number of columns will remain the same.
#'
#'
compute_weighted_prevalence <- function(df, .edema=NULL, .summary_by = NULL) {
  .summary_by <- rlang::enquo(.summary_by)

  if (!is.null(.summary_by)) {
    df <- df |>
      filter(.data$flag_mfaz == 0) |>
      #mutate(muac = recode_muac(.data$muac, unit = "cm")) |>
      summarise(
        sam = apply_cdc_age_weighting(.data$muac, .data$age, {{ .edema }}, status = "sam"),
        mam = apply_cdc_age_weighting(.data$muac, .data$age, {{ .edema }}, status = "mam"),
        gam = sum(.data$sam, .data$mam),
        .by = !!.summary_by
      ) |>
      dplyr::rename(
        gam_p = .data$gam, sam_p = .data$sam, mam_p = .data$mam
      )
  } else {
    df <- df |>
      filter(.data$flag_mfaz == 0) |>
      mutate(muac = recode_muac(.data$muac, unit = "mm")) |>
      summarise(
        sam = apply_cdc_age_weighting(.data$muac, .data$age, {{ .edema }}, status = "sam"),
        mam = apply_cdc_age_weighting(.data$muac, .data$age, {{ .edema }}, status = "mam"),
        gam = sum(.data$sam, .data$mam)
      ) |>
      dplyr::rename(
        gam_p = .data$gam, sam_p = .data$sam, mam_p = .data$mam
      )
  }
  df
}



#'
#'
#'
#'
compute_pps_based_muac_prevalence <- function(df,
                                              .wt = NULL,
                                              .edema = NULL,
                                              .summary_by = NULL) {
  df <- df |>
    define_wasting(muac = .data$muac, edema = {{ .edema }}, base = "muac")

  ### Weighted survey analysis ----
  if (!is.null(.wt)) {
    srvy <- df |>
      as_survey_design(
        ids = .data$cluster,
        pps = "brewer",
        variance = "YG",
        weights = {{ .wt }}
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
      wt_pop = sum(srvyr::cur_svy_wts())
    )
  p
}


#'
#'
#' @rdname prevalence
#'
#' @examples
#'
#' ## An example of application of `compute_muac_prevalence()` ----
#'
#' ### When .summary.by = NULL ----
#'
#' x <- compute_muac_prevalence(
#' df = anthro.04,
#' .wt = NULL,
#' .edema = edema,
#' .summary_by = NULL
#' )
#'
#' print(x)
#'
#' ### When .summary_by is not set to NULL ----
#'
#' p <- compute_muac_prevalence(
#' df = anthro.04,
#' .wt = NULL,
#' .edema = edema,
#' .summary_by = province
#' )
#'
#' print(p)
#'
#' @export
#'
compute_muac_prevalence <- function(df,
                                    .wt = NULL,
                                    .edema = NULL,
                                    .summary_by = NULL) {

  ## Difuse argument .summary_by ----
  .summary_by <- rlang::enquo(.summary_by)

  ## An empty vector type list ----
  results <- list()

  if (!rlang::quo_is_null(.summary_by)) {
    ## Grouped summary of analysis approach ----
    x <- df |>
      group_by(!!.summary_by) |>
      summarise(
        age_ratio = classify_age_sex_ratio(age_ratio_test(.data$age, .expectedP = 0.66)$p),
        std = classify_sd(sd(remove_flags(as.numeric(.data$mfaz), "zscore"), na.rm = TRUE)),
        analysis_approach = tell_muac_analysis_strategy(.data$age_ratio, .data$std),
        .groups = "drop"
      )
  } else {
    ## Non-grouped summary of analysis approach ----
    x <- df |>
      summarise(
        age_ratio = classify_age_sex_ratio(age_ratio_test(.data$age, .expectedP = 0.66)$p),
        std = classify_sd(sd(remove_flags(as.numeric(.data$mfaz), "zscore"), na.rm = TRUE)),
        analysis_approach = tell_muac_analysis_strategy(.data$age_ratio, .data$std)
      )
  }

  ## Iterate over data frame to compute prevalence according to analysis_approach ----
  for (i in seq_len(nrow(x))) {
    if (!rlang::quo_is_null(.summary_by)) {
      area <- dplyr::pull(x, !!.summary_by)[i]
      data <- filter(df, !!sym(rlang::quo_name(.summary_by)) == area)
    } else {
      data <- df
    }

    analysis_approach <- x$analysis_approach[i]

    if (analysis_approach == "unweighted") {
      ### Compute standard complex sample based prevalence analysis ----
      output <- compute_pps_based_muac_prevalence(data, {{ .wt }}, {{ .edema }}, !!.summary_by)
    } else if (analysis_approach == "weighted") {
      ### Compute grouped weighted prevalence ----
      if (!rlang::quo_is_null(.summary_by)) {
        output <- compute_weighted_prevalence(data, .edema = {{ .edema }}, !!.summary_by)
      } else {
        ### Compute grouped weighted prevalence ----
        output <- compute_weighted_prevalence(data, .edema = {{ .edema }})
      }
    } else {
      ## Add grouped NA's ----
      if (!rlang::quo_is_null(.summary_by)) {
        output <- summarise(
          data,
          gam_p = NA_real_,
          sam_p = NA_real_,
          mam_p = NA_real_,
          .by = !!.summary_by
        )
      } else {
        ## Add non-grouped NA's ----
        output <- tibble::tibble(
          gam_p = NA_real_,
          sam_p = NA_real_,
          mam_p = NA_real_
        )
      }
    }
    results[[i]] <- output
  }
  ### Ensure that all geographical areas are added to the tibble ----
  if (!rlang::quo_is_null(.summary_by)) {
    results <- dplyr::bind_rows(results) |>
      dplyr::relocate(.data$gam_p, .after = .data$gam_n) |>
      dplyr::relocate(.data$sam_p, .after = .data$sam_n) |>
      dplyr::relocate(.data$mam_p, .after = .data$mam_n)
  } else {
    ## Non-grouped results
    results <- dplyr::bind_rows(results)
  }
  results
}
