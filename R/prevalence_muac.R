#'
#' A helper function to determine the MUAC prevalence analysis approach to follow
#'
#' @description
#' It determines the analysis approach to follow for a given analysis area on
#' the basis of the rate of acceptability of the age ratio test and the standard
#' deviation analysis result.
#'
#' @param age_ratio_class A vector of class `character` of the acceptability
#' classification of the age ratio test result.
#'
#' @param sd_class A vector of class `character` of the acceptability
#' classification of the standard deviation analysis result.
#'
#' @returns A vector of class `character` of the same length as the input vectors,
#' containing values indicating the analysis approach for each analysis area: "weighted",
#' "unweighted" and "missing".
#'
#' @details
#' When "weighted", the CDC weighting approach is applied to correct for
#' age bias; when "unweighted" a normal complex sample analysis is applied; when
#' "missing" `NA` gets thrown.
#'
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
#' Apply the CDC/SMART prevalence weighting approach on MUAC data
#'
#' @description
#' Calculate a weighted prevalence estimate of MUAC by adding the proportion of
#' children under 2 years to twice the proportion of children over 2 and then
#' dividing by 3.
#'
#' @param muac A vector of class `integer` of MUAC values (in mm).
#'
#' @param age A vector of class `double` of child's age in months.
#'
#' @param .edema A vector of class `character` of edema. Code should be
#' "y" for presence and "n" for absence of bilateral edema. Default is `NULL`.
#'
#' @param status A choice of the form of wasting to be defined.
#'
#' @returns A vector of class `numeric` of length and size 1.
#'
#' @details
#' This function is informed by the output of [age_ratio_test()].
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
#' Apply the CDC/SMART prevalence weighting approach on MUAC data
#'
#' @param df An already wrangled dataset object of class `data.frame` to use.
#'
#' @param .edema A vector of class `character` of edema. Code should be
#' "y" for presence and "n" for absence of bilateral edema. Default is `NULL`.
#'
#' @param .summary_by A vector of class `character` of the geographical areas
#' where the data was collected and for which the analysis should be performed.
#'
#' @returns A table of class `data.frame` of dimensions that vary based on
#' `.summary_by`, containing the results.
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
