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
        cmam <- ifelse(
          (zscores >= -3 & zscores < -2) | 
            (muac >= 115 & muac < 125) & 
            edema == "n", 1, 0
        )
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
        cmam <- ifelse(
          (zscores >= -3 & zscores < -2) | 
            (muac >= 115 & muac < 125), 1, 0
        )
      }
    )
  }
}


#'
#' Define wasting
#'
#' @description
#' Determine if a given observation in the data set is wasted or not, and its
#' respective form of wasting (global, severe or moderate) on the basis of
#' z-scores of weight-for-height (WFHZ), muac-for-age (MFAZ), unadjusted MUAC 
#' values and combined case-definition.
#'
#' @param df A `tibble` object. It must have been wrangled using this package's 
#' wrangling functions for WFHZ or MUAC, or both (for combined) as appropriate.
#'
#' @param zscores A vector of class `double` of WFHZ or MFAZ values.
#'
#' @param muac An `integer` or `character` vector of unadjusted MUAC values in
#' millimeters.
#'
#' @param edema A `character` vector indicating edema status. Default is NULL.
#' Code values should be "y" for presence and "n" for absence of nutritional 
#' edema.
#'
#' @param .by A choice of the criterion by which a case is to be defined. Choose 
#' "zscores" for WFHZ or MFAZ, "muac" for raw MUAC and "combined" for combined.
#' Default value is "zscores".
#'
#' @returns The `tibble` object `df` with additional columns named named `gam`, 
#' `sam` and `mam`, each of class `numeric` containing coded values of either 
#' 1 (case) and 0 (not a case). If `.by = "combined"`, additional columns are
#' named `cgam`, `csam` and `cmam`.
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
  zscores <- rlang::eval_tidy(enquo(zscores), df)
  muac <- rlang::eval_tidy(enquo(muac), df)
  edema <- rlang::eval_tidy(enquo(edema), df)

  ## Enforce options in `.by` ----
  .by <- match.arg(.by)

  ## Enforce class of `zscores` ----
  if(!is.null(zscores)) {
    if (!is.double(zscores)) {
      stop(
        "`zscores` must be of class double not ", 
        class(zscores), ". Please try again."
      )
    }
  }

  ## Enforce class of `muac` ----
  if(!is.null(muac)) {
    if (!(is.numeric(muac) | is.integer(muac))) {
      stop(
        "`muac` must be of class numeric or integer not ", 
        class(muac), ". Please try again."
      )
    }
  }

  ## Enforce class of `edema` ----
  if(!is.null(edema)) {
    if (!is.character(edema)) {
      stop(
        "`edema` must be of class character not ", 
        class(edema), ". Please try again."
      )
    }
    ## Enforce code values in `edema` ----
    if (!(all(levels(as.factor(as.character(edema))) %in% c("y", "n")))) {
      stop('Values in `edema` should either be "y" or "n". Please try again.')
    }
  }

  ## Define cases ----
  switch(
    ### By WFHZ or MFAZ and add to the data frame ----
    .by,
    "zscores" = {
      dplyr::mutate(
        .data = df,
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
      dplyr::mutate(
        .data = df,
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
      dplyr::mutate(
        .data = df,
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
smart_tool_case_definition <- function(muac, edema = NULL) {
  if (!is.null(edema)) {
    ## Define cases including edema ----
    x <- dplyr::case_when(
      muac < 115 | {{ edema }} == "y" ~ "sam",
      muac >= 115 & muac < 125 & {{ edema }} == "n" ~ "mam",
      .default = "not wasted"
    )
  } else {
    ## Define cases excluding edema ----
    x <- dplyr::case_when(
      muac < 115 ~ "sam",
      muac >= 115 & muac < 125 ~ "mam",
      .default = "not wasted"
    )
  }
  x
}
