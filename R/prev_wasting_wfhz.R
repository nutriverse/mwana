#'
#'
#' @keywords internal
#'
#'
complex_survey_estimates_wfhz <- function(df,
                                         wt = NULL,
                                         edema = NULL,
                                         .by) {

  ## Difuse ----
  wt <- enquo(wt)

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
  if (!quo_is_null(wt)) {
    srvy <- df |>
      as_survey_design(
        ids = .data$cluster,
        pps = "brewer",
        variance = "YG",
        weights = !!wt
      )
  } else {
    ## Create survey object ----
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
#' Estimate the prevalence of wasting based on z-scores of weight-for-height (WFHZ)
#'
#' @description
#' Calculate the prevalence estimates of wasting based on z-scores of
#' weight-for-height and/or bilateral edema. The function allows users to
#' get the prevalence estimates calculated in accordance with the complex sample
#' design properties; this includes applying survey weights when needed or applicable.
#' When the standard deviation of WFHZ is rated as problematic, the prevalence is
#' estimated based on the PROBIT method. This is as in the SMART Methodology.
#'
#' @param df A dataset object of class `data.frame` to use. This must have been
#' wrangled using this package's wrangling function for WFHZ data. The function
#' uses a variable name called `cluster` where the primary sampling unit IDs
#' are stored. Make sure to rename your cluster ID variable to `cluster`, otherwise
#' the function will error and terminate the execution.
#'
#' @param wt A vector of class `double` of the final survey weights. Default is
#'  `NULL` assuming a self weighted survey, as in the ENA for SMART software;
#'  otherwise, when a vector of weights if supplied, weighted analysis is done.
#'
#' @param edema A vector of class `character` of edema. Code should be
#' "y" for presence and "n" for absence of bilateral edema. Default is `NULL`.
#'
#' @param .by A vector of class `character` or `numeric` of the geographical areas
#' or respective IDs for where the data was collected and for which the analysis
#' should be summarised at.
#'
#' @returns A summarised table of class `data.frame` of the descriptive
#' statistics about wasting.
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
#'   edema = edema,
#'   .by = NULL
#' )
#'
#' ## Now when .by is not set to NULL ----
#' mw_estimate_prevalence_wfhz(
#'   df = data,
#'   wt = NULL,
#'   edema = edema,
#'   .by = district
#' )
#'
#' ## When a weighted analysis is needed ----
#' mw_estimate_prevalence_wfhz(
#'   df = anthro.02,
#'   wt = wtfactor,
#'   edema = edema,
#'   .by = province
#' )
#'
#' @export
#'
mw_estimate_prevalence_wfhz <- function(df,
                                        wt = NULL,
                                        edema = NULL,
                                        .by = NULL) {

  ## Difuse argument `.by` ----
  .by <- enquo(.by)

  ## Empty vector type list ----
  results <- list()

  if (!quo_is_null(.by)) {
    ## Rate standard deviation ----
    x <- df |>
      summarise(
      std = rate_std(sd(remove_flags(.data$wfhz, "zscores"), na.rm = TRUE)),
      .by = !!.by
    )
  } else {
    ## Rate standard deviation ----
    x <- df |>
      summarise(
      std = rate_std(sd(remove_flags(.data$wfhz, "zscores"), na.rm = TRUE))
    )
  }

  ## Compute prevalence based on the rate of the SD ----
  for (i in seq_len(nrow(x))) {
    if (!quo_is_null(.by)) {
      area <- pull(x, !!.by)[i]
      data <- filter(df, !!sym(quo_name(.by)) == !!area)
    } else {
      data <- df
    }

    std <- x$std[i]
    if (std != "Problematic") {
      ### Compute complex sample-based prevalence estimates ----
      result <- data |>
        complex_survey_estimates_wfhz(
          wt = {{ wt }},
          edema = {{ edema }},
          .by = !!.by
          )
    } else {
      ### Compute PROBIT-based prevalence estimates----
      if (!quo_is_null(.by)) {
        result <- data |>
          estimate_probit_prevalence(
            .by = !!.by,
            .for = "wfhz"
            )
      } else {
        ### Compute PROBIT-based prevalence estimates ----
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
