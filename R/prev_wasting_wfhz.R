#'
#'
#' @keywords internal
#'
#'
complex_survey_estimates_wfhz <- function(df,
                                          wt = NULL,
                                          edema = NULL,
                                          ...) {
  ## Difuse arguments ----
  wt <- rlang::enquo(wt)
  edema <- rlang::enquo(edema)
  .by <- rlang::enquos(...)

  ## Defines case based on the availability of edema ----
    df <- define_wasting(
      df,
      zscores = .data$wfhz,
      edema = !!edema,
      .by = "zscores"
    )

  ## Filter out flags ----
  df <- dplyr::filter(.data = df, .data$flag_wfhz == 0)

  ## Create a survey object for a weighted analysis ----
    srvy <- srvyr::group_by(df, !!!.by) |> 
      srvyr::as_survey_design(
      ids = .data$cluster,
      pps = "brewer",
      variance = "YG",
      weights = !!wt
    )

  ## Summarise prevalence ----
  p <- srvyr::summarise(
    .data = srvy,
      srvyr::across(
        .data$gam:.data$mam,
        list(
          n = ~sum(.x, na.rm = TRUE),
          p = \(.) srvyr::survey_mean(
            .,
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
#' Estimate the prevalence of wasting based on weight-for-height z-scores (WFHZ)
#'
#' @description
#' Calculate the prevalence estimates of wasting based on z-scores of
#' weight-for-height and/or nutritional edema. The function allows users to
#' estimate prevalence in accordance with complex sample design properties such
#' as accounting for survey sample weights when needed or applicable. The
#' quality of the data is first evaluated by calculating and rating the standard
#' deviation of WFHZ. Standard approach to prevalence estimation is calculated
#' only when the standard deviation of MFAZ is rated as not problematic. If
#' the standard deviation is problematic, prevalence is estimated using the
#' PROBIT estimator. Outliers are detected based on SMART flagging criteria.
#' Identified outliers are then excluded before prevalence estimation is
#' performed.
#'
#' @param df A `tibble` object that has been produced by the [mw_wrangle_wfhz()]
#' functions. The `df` should have a variable named `cluster` for the primary
#' sampling unit identifiers.
#'
#' @param wt A vector of class `double` of the survey sampling weights. Default
#' is NULL which assumes a self-weighted survey as is the case for a survey
#' sample selected proportional to population size (i.e., SMART survey sample).
#' Otherwise, a weighted analysis is implemented.
#'
#' @param edema A `character` vector for presence of nutritional edema coded as
#' "y" for presence of nutritional edema and "n" for absence of nutritional
#' edema. Default is NULL.
#'
#' @param ... A vector of class `character`, specifying the categories for which
#' the analysis should be summarised for. Usually geographical areas. More than
#' one vector can be specified.
#'
#' @returns A summary `tibble` for the descriptive statistics about wasting.
#'
#' @examples
#' ## When .by = NULL ----
#' ### Start off by wrangling the data ----
#' data <- mw_wrangle_wfhz(
#'   df = anthro.03,
#'   sex = sex,
#'   weight = weight,
#'   height = height,
#'   .recode_sex = TRUE
#' )
#'
#' ### Now run the prevalence function ----
#' mw_estimate_prevalence_wfhz(
#'   df = data,
#'   wt = NULL,
#'   edema = edema
#' )
#'
#' ## Now when .by is not set to NULL ----
#' mw_estimate_prevalence_wfhz(
#'   df = data,
#'   wt = NULL,
#'   edema = edema,
#'   district
#' )
#'
#' ## When a weighted analysis is needed ----
#' mw_estimate_prevalence_wfhz(
#'   df = anthro.02,
#'   wt = wtfactor,
#'   edema = edema,
#'   province
#' )
#'
#' @export
#'

mw_estimate_prevalence_wfhz <- function(df,
                                        wt = NULL,
                                        edema = NULL,
                                        ...) {
  ## Defuse argument `.by` ----
  .by <- rlang::enquos(...)

  ## Empty vector type list ----
  results <- list()

  ## Apply grouping if needed ----
  if (length(.by) > 0)  df <- dplyr::group_by(df, !!!.by)
  
    ## Rate standard deviation ----
    d <- dplyr::summarise(
      .data = df,
      std = rate_std(stats::sd(remove_flags(.data$wfhz, "zscores"), na.rm = TRUE)),
      .groups = "keep"
    )

  ## Compute prevalence based on the rate of the SD ----
  for (i in seq_len(nrow(d))) {
     if (length(.by) > 0) {
      vals <- purrr::map(.by, ~ dplyr::pull(d, !!.x)[i])
      exprs <- purrr::map2(.by, vals, ~ rlang::expr(!!rlang::get_expr(.x) == !!.y))
      data_subset <- dplyr::filter(df, !!!exprs)
    } else {
      data_subset <- df
    }

    std <- d$std[i]
    if (std != "Problematic") {
      ### Compute complex sample-based prevalence estimates ----
      result <- complex_survey_estimates_wfhz(
        data_subset,
        wt = {{ wt }},
        edema = {{ edema }},
        !!!.by
      )
    } else {
      ### Compute PROBIT-based prevalence estimates----
      if (length(.by) > 0) {
        result <- estimate_probit_prevalence(
          data_subset,
          .for = "wfhz",
          !!!.by
        )
      } else {
        ### Compute PROBIT-based prevalence estimates ----
        result <- estimate_probit_prevalence(data_subset, .for = "wfhz")
      }
    }

    results[[i]] <- result
  }

  dplyr::bind_rows(results) |>
    dplyr::relocate(.data$gam_p, .after = .data$gam_n) |>
    dplyr::relocate(.data$sam_p, .after = .data$sam_n) |>
    dplyr::relocate(.data$mam_p, .after = .data$mam_n)
}