#'
#'
#' @keywords internal
#'
#'
define_wasting_muac <- function(muac,
                                edema = NULL,
                                .cases = c("gam", "sam", "mam")) {
  ## Enforce options in `.cases` ----
  .cases <- match.arg(.cases)

  if (!is.null(edema)) {
    switch(
      ### Wasting by MUAC including MUAC ----
      .cases,
      "gam" = {
        gam <- ifelse(muac < 125 | edema == "y", 1, 0)
      },
      "sam" = {
        sam <- ifelse(muac < 115 | edema == "y", 1, 0)
      },
      "mam" = {
        mam <- ifelse((muac >= 115 & muac < 125 & edema == "n"), 1, 0)
      }
    )
  } else {
    switch(
      ### Wasting by MUAC without edema ----
      .cases,
      "gam" = {
        gam <- ifelse(muac < 125, 1, 0)
      },
      "sam" = {
        sam <- ifelse(muac < 115, 1, 0)
      },
      "mam" = {
        mam <- ifelse((muac >= 115 & muac < 125), 1, 0)
      }
    )
  }
}

#'
#'
#' @keywords internal
#'
#'
define_wasting_zscores <- function(zscores,
                                edema = NULL,
                                .cases = c("gam", "sam", "mam")) {
  ## Enforce options in `.cases` ----
  .cases <- match.arg(.cases)

  if (!is.null(edema)) {
    switch(
      ### Wasting by WFHZ including edema ----
      .cases,
      "gam" = {
        gam <- ifelse(zscores < -2 | edema == "y", 1, 0)
      },
      "sam" = {
        sam <- ifelse(zscores < -3 | edema == "y", 1, 0)
      },
      "mam" = {
        mam <- ifelse((zscores >= -3 & zscores < -2 & edema == "n"), 1, 0)
      }
    )
  } else {
    switch(
      ### Wasting by MFHZ sem edema ----
      .cases,
      "gam" = {
        gam <- ifelse(zscores < -2, 1, 0)
      },
      "sam" = {
        sam <- ifelse(zscores < -3, 1, 0)
      },
      "mam" = {
        mam <- ifelse(zscores >= -3 & zscores < -2, 1, 0)
      }
    )
  }
}

#'
#'
#' @keywords internal
#'
#'
define_wasting_combined <- function(zscores,
                                    muac,
                                    edema = NULL,
                                    .cases = c("cgam", "csam", "cmam")) {
  ## Enforce options in `.cases` ----
  .cases <- match.arg(.cases)

  if (!is.null(edema)) {
    switch(
      ### Combined wasting including edema ----
      .cases,
      "cgam" = {
        cgam <- ifelse(zscores < -2 | muac < 125 | edema == "y", 1, 0)
      },
      "csam" = {
        csam <- ifelse(zscores < -3 | muac < 115 | edema == "y", 1, 0)
      },
      "cmam" = {
        cmam <- ifelse((zscores >= -3 & zscores < -2) | (muac >= 115 & muac < 125) & (edema == "n"), 1, 0)
      }
    )
  } else {
    switch(
      ### Combined wasting without edema ----
      .cases,
      "cgam" = {
        cgam <- ifelse(zscores < -2 | muac < 125, 1, 0)
      },
      "csam" = {
        csam <- ifelse(zscores < -3 | muac < 115, 1, 0)
      },
      "cmam" = {
        cmam <- ifelse((zscores >= -3 & zscores < -2) | (muac >= 115 & muac < 125), 1, 0)
      }
    )
  }
}


#'
#' Define wasting
#'
#' @description
#' Define if a given observation in the dataset is wasted or not, and its
#' respective form of wasting (global, severe or moderate) on the basis of
#' z-scores of weight-for-height (WFHZ), muac-for-age (MFAZ), raw MUAC values and
#' combined case-definition.
#'
#' @param df A dataset object of class `data.frame` to use. It must have been
#' wrangled using this package's wrangling functions for WFHZ or MUAC, or both
#' (for combined) as appropriate.
#'
#' @param zscores A vector of class `double` of WFHZ or MFAZ values. If the class
#' does not match the expected type, the function will stop execution and return
#' an error message indicating the type of mismatch.
#'
#' @param muac A vector of class `integer` or `numeric` of raw MUAC values in
#' millimeters. If the class does not match the expected type, the function will
#' stop execution and return an error message indicating the type of mismatch.
#'
#' @param edema A vector of class `character` of edema. Default is `NULL`.
#' If the class does not match the expected type, the function will stop execution
#' and return an error message indicating the type of mismatch. Code values should be
#' "y" for presence and "n" for absence of bilateral edema. If different, the
#' function will stop execution and return an error indicating the issue.
#'
#' @param .by A choice of the criterion by which the case-definition should done.
#' Choose `zscores` for WFHZ or MFAZ, `muac` for raw MUAC and `combined` for
#' combined.
#'
#' @returns Three vectors named `gam`, `sam` and `mam`, of class `numeric`, same
#' length as inputs, containing dummy values: 1 for case and 0 for not case.
#' This is added to `df`. When `combined` is selected, vector's names become
#' `cgam`, `csam` and `cmam`.
#'
#' @examples
#' ## Case-definition by z-scores ----
#' z <- anthro.02 |>
#'   define_wasting(
#'     zscores = wfhz,
#'     muac = NULL,
#'     edema = edema,
#'     .by = "zscores"
#'   )
#' head(z)
#'
#' ## Case-definition by MUAC ----
#' m <- anthro.02 |>
#'   define_wasting(
#'     zscores = NULL,
#'     muac = muac,
#'     edema = edema,
#'     .by = "muac"
#'   )
#' head(m)
#'
#' ## Case-definition by combined ----
#' c <- anthro.02 |>
#'   define_wasting(
#'     zscores = wfhz,
#'     muac = muac,
#'     edema = edema,
#'     .by = "combined"
#'   )
#' head(c)
#'
#' @export
#'
define_wasting <- function(df,
                           zscores = NULL,
                           muac = NULL,
                           edema = NULL,
                           .by = c("zscores", "muac", "combined")) {

  ## Difuse and evaluate arguments ----
  zscores <- eval_tidy(enquo(zscores), df)
  muac <- eval_tidy(enquo(muac), df)
  edema <- eval_tidy(enquo(edema), df)

  ## Enforce options in `.by` ----
  .by <- match.arg(.by)

  ## Enforce class of `zscores` ----
  if(!is.null(zscores)) {
    if (!is.double(zscores)) {
      stop("`zscores` must be of class 'double'; not ", shQuote(class(zscores)), ". Please try again.")
    }
  }

  ## Enforce class of `muac` ----
  if(!is.null(muac)) {
    if (!(is.numeric(muac) | is.integer(muac))) {
      stop("`muac` must be of class 'numeric' or 'integer'; not ", shQuote(class(muac)), ". Please try again.")
    }
  }

  ## Enforce class of `edema` ----
  if(!is.null(edema)) {
    if (!is.character(edema)) {
      stop("`edema` must be of class 'character'; not ", shQuote(class(edema)), ". Please try again.")
    }
    ## Enforce code values in `edema` ----
    if (!(all(levels(as.factor(as.character(edema))) %in% c("y", "n")))) {
      stop("Values in `edema` should either be 'y' or 'n'. Please try again.")
    }
  }

  ## Define cases ----
  switch(
    ### By WFHZ or MFAZ and add to the data frame ----
    .by,
    "zscores" = {
      df |>
        mutate(
          gam = define_wasting_zscores(
            zscores = {{ zscores }},
            edema = {{ edema }},
            .cases = "gam"
          ),
          sam = define_wasting_zscores(
            zscores = {{ zscores }},
            edema = {{ edema }},
            .cases = "sam"
          ),
          mam = define_wasting_zscores(
            zscores = {{ zscores }},
            edema = {{ edema }},
            .cases = "mam"
          )
        )
    },
    ### By MUAC and add to the data frame ----
    "muac" = {
      df |>
        mutate(
          gam = define_wasting_muac(
            muac = {{ muac }},
            edema = {{ edema }},
            .cases = "gam"
          ),
          sam = define_wasting_muac(
            muac = {{ muac }},
            edema = {{ edema }},
            .cases = "sam"
          ),
          mam = define_wasting_muac(
            muac = {{ muac }},
            edema = {{ edema }},
            .cases = "mam"
          )
        )
    },
    ### By combined add to the data frame ----
    "combined" = {
      df |>
        mutate(
          cgam = define_wasting_combined(
            zscores = {{ zscores }},
            muac = {{ muac }},
            edema = {{ edema }},
            .cases = "cgam"
          ),
          csam = define_wasting_combined(
            zscores = {{ zscores }},
            muac = {{ muac }},
            edema = {{ edema }},
            .cases = "csam"
          ),
          cmam = define_wasting_combined(
            zscores = {{ zscores }},
            muac = {{ muac }},
            edema = {{ edema }},
            .cases = "cmam"
          )
        )
    }
  )
}



#'
#'
#' @keywords internal
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
