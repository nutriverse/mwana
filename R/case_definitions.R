#'
#' Define wasting based on WFHZ, MFAZ, MUAC and Combined criteria
#'
#' @param df A dataset object of class `data.frame` to use.
#'
#' @param muac A vector of class `integer` of MUAC values in millimeters.
#'
#' @param zscore A vector of class `double` of WFHZ values (with 3 decimal places).
#'
#' @param edema A vector of class `character` of edema. Code should be
#' "y" for presence and "n" for absence of bilateral edema. Default is `NULL`.
#'
#' @param cases A choice of the form of wasting to be defined.
#'
#' @param base A choice of the criterion on which the case-definition should be based.
#'
#' @returns A vector of class `numeric` of dummy values: 1 for case and 0
#' for not case.
#'
#' @details
#' Use `define_wasting()` to add the case-definitions to data frame.
#'
#' @rdname case_definition
#'
#'
define_wasting_cases_muac <- function(muac, edema = NULL,
                               cases = c("gam", "sam", "mam")) {
  ## Match argument ----
  cases <- match.arg(cases)

  if (!is.null(edema)) {
    switch(
      ### Define cases based on MUAC including edema ----
      cases,
      "gam" = {gam <- ifelse(muac < 125 | edema == "y", 1, 0)},
      "sam" = {sam <- ifelse(muac < 115 | edema == "y", 1, 0)},
      "mam" = {mam <- ifelse((muac >= 115 & muac < 125 & edema == "n"), 1, 0)}
    )
  } else {
    switch(
      ### Define cases based on MUAC ----
      cases,
      "gam" = {gam <- ifelse(muac < 125, 1, 0)},
      "sam" = {sam <- ifelse(muac < 115, 1, 0)},
      "mam" = {mam <- ifelse((muac >= 115 & muac < 125), 1, 0)}
    )
  }
}

#'
#'
#' @rdname case_definition
#'
#'
define_wasting_cases_whz <- function(zscore, edema = NULL,
                              cases = c("gam", "sam", "mam")) {
  ## Match argument ----
  cases <- match.arg(cases)

  if (!is.null(edema)) {
    switch(
      ### Define cases based on WFHZ including edema ----
      cases,
      "gam" = {gam <- ifelse(zscore < -2 | edema == "y", 1, 0)},
      "sam" = {sam <- ifelse(zscore < - 3 | edema == "y", 1, 0)},
      "mam" = {mam <- ifelse((zscore >= -3 & zscore < -2 & edema == "n"), 1, 0)}
    )
  } else {
    switch(
      ### Define cases based on WFHZ ----
      cases,
      "gam" = {gam <- ifelse(zscore < -2, 1, 0)},
      "sam" = {sam <- ifelse(zscore < - 3, 1, 0)},
      "mam" = {mam <- ifelse(zscore >= -3 & zscore < -2, 1, 0)}
    )
  }
}

#'
#'
#' @rdname case_definition
#'
#'
define_wasting_cases_combined <- function(zscore, muac, edema = NULL,
                                   cases = c("cgam", "csam", "cmam")) {

  ## Match argument ----
  cases <- match.arg(cases)

  if (!is.null(edema)) {
    switch(
      ### Define cases based on WFHZ or MUAC or edema ----
      cases,
      "cgam" = {cgam <- ifelse(zscore < -2 | muac < 125 | edema == "y", 1, 0)},
      "csam" = {csam <- ifelse(zscore < -3 | muac < 115 | edema == "y", 1, 0)},
      "cmam" = {cmam <- ifelse((zscore >= -3 & zscore < -2) | (muac >= 115 & muac < 125) & (edema == "n"), 1, 0)}
    )
  } else {
    switch(
      ### Define cases based on WFHZ or MUAC ----
      cases,
      "cgam" = {cgam <- ifelse(zscore < -2 | muac < 125, 1, 0)},
      "csam" = {csam <- ifelse(zscore < -3 | muac < 115, 1, 0)},
      "cmam" = {cmam <- ifelse((zscore >= -3 & zscore < -2) | (muac >= 115 & muac < 125), 1, 0)}
    )
  }
}


#'
#' @examples
#'
#' ## Weight-for-height based case-definition ----
#' x <- anthro.02 |>
#' define_wasting(
#' zscore = wfhz,
#' edema = edema,
#' base = "wfhz"
#' )
#' head(x)
#'
#' ## MUAC-based case-definition ----
#' x <- anthro.02 |>
#' define_wasting(
#' muac = muac,
#' edema = edema,
#' base = "muac"
#' )
#' head(x)
#'
#' ## Combined case-definition ----
#' x <- anthro.02 |>
#' define_wasting(
#' zscore = wfhz,
#' muac = muac,
#' edema = edema,
#' base = "combined"
#' )
#' head(x)
#'
#' @rdname case_definition
#'
#' @export
#'
define_wasting <- function(df, zscore = NULL, muac = NULL, edema = NULL,
                           base = c("wfhz", "muac", "combined")) {

  ## Match argument ----
  base <- match.arg(base)

  switch(
    ### Add WFHZ based case definitions to data frame ----
    base,
    "wfhz" = {
      df |>
        mutate(
          gam = define_wasting_cases_whz(
            zscore = {{ zscore }},
            edema = {{ edema }},
            cases = "gam"
            ),
          sam = define_wasting_cases_whz(
            zscore = {{ zscore }},
            edema = {{ edema }},
            cases = "sam"
            ),
          mam = define_wasting_cases_whz(
            zscore = {{ zscore }},
            edema = {{ edema }},
            cases = "mam")
        )
    },
    ### Add MUAC based case definitions to data frame ----
    "muac" = {
      df |>
        mutate(
          gam = define_wasting_cases_muac(
            muac = {{ muac }},
            edema = {{ edema }},
            cases = "gam"
            ),
          sam = define_wasting_cases_muac(
            muac = {{ muac }},
            edema = {{ edema }},
            cases = "sam"
          ),
          mam = define_wasting_cases_muac(
            muac = {{ muac }},
            edema = {{ edema }},
            cases = "mam"
            )
        )
    },
    ### Add combined (WFHZ or MUAC or edema) based case definitions to data frame ----
    "combined" = {
      df |>
        mutate(
          cgam = define_wasting_cases_combined(
            zscore = {{ zscore }},
            muac = {{ muac }},
            edema = {{ edema }},
            cases = "cgam"
          ),
          csam = define_wasting_cases_combined(
            zscore = {{ zscore }},
            muac = {{ muac }},
            edema = {{ edema }},
            cases = "csam"
          ),
          cmam = define_wasting_cases_combined(
            zscore = {{ zscore }},
            muac = {{ muac }},
            edema = {{ edema }},
            cases = "cmam"
            )
        )
    }
  )
}

#'
#' Classify wasting into severe or moderate wasting to be used in the
#' SMART MUAC tool weighting approach
#'
#' @param muac A vector of class `integer` of MUAC values in millimeters.
#'
#' @param .edema A vector of class `character` of edema. Code should be
#' "y" for presence and "n" for absence of bilateral edema. Default is `NULL`.
#'
#' @returns A vector of class `character` of the same length as `muac` and `.edema`
#'  indicating if a child is severe or moderately wasted or not wasted.
#'
#'
classify_wasting_for_cdc_approach <- function(muac, .edema = NULL) {
  if (!is.null(.edema)) {
    ## Define cases including edema ----
    x <- case_when(
      muac < 115 | {{ .edema }} == "y" ~ "sam",
      muac >= 115 & muac < 125 & {{ .edema }} == "n" ~ "mam",
      .default = "not wasted"
    )
  } else {
    ## Define cases excluding edema ----
    x <- case_when(
      muac < 115 ~ "sam",
      muac >= 115 & muac < 125 ~ "mam",
      .default = "not wasted"
    )
  }
  x
}
