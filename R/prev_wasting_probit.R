#'
#'
#'
#' @keywords internal
#'
#'
apply_probit_method <- function(x, .status = c("gam", "sam")) {

  ## Enforce options in `.status` ----
  .status <- match.arg(.status)

  ## Calculate mean of zscores ----
  mean_zscore <- mean(remove_flags(x, "zscores"), na.rm = TRUE)

  ## Estimate prevalence based on probit method, with a SD = 1 ----
  switch(
    .status,
    "gam" = {
      pnorm(
        q = -2, mean = mean_zscore, sd = 1, lower.tail = TRUE, log.p = FALSE
      )
    },
    "sam" = {
      pnorm(
        q = -3, mean = mean_zscore, sd = 1, lower.tail = TRUE, log.p = FALSE
      )
    }
  )
}



#'
#'
#' @keywords internal
#'
#'
estimate_probit_prevalence <- function(df,
                                      .by = NULL,
                                      .for = c("wfhz", "mfaz")) {

  ## Defuse argument ----
  .by <- enquo(.by)

  ## Enforce options in `.for` ----
  .for <- match.arg(.for)

  ## Calculate probit-based prevalence ----
  switch(
    .for,
    "wfhz" = {
      if(!is.null(.by)) {
        df <- df |>
          summarise(
            gam = apply_probit_method(.data$wfhz, .status = "gam"),
            sam = apply_probit_method(.data$wfhz, .status = "sam"),
            mam = .data$gam - .data$sam,
            .by = !!.by
          ) |>
          mutate(
            gam_p = .data$gam,
            sam_p = .data$sam,
            mam_p = .data$mam,
            gam = NA,
            sam = NA,
            mam = NA
          ) |>
          select(!2:4) # Make it fit in structure of the returned df in the main function
      } else {
        df <- df |>
          summarise(
            gam = apply_probit_method(.data$wfhz, .status = "gam"),
            sam = apply_probit_method(.data$wfhz, .status = "sam"),
            mam = .data$gam - .data$sam
          ) |>
          mutate(
            gam_p = .data$gam,
            sam_p = .data$sam,
            mam_p = .data$mam,
            gam = NA,
            sam = NA,
            mam = NA
          ) |>
          select(!2:4) # Make it fit in structure of the returned df in the main function
      }
      df
    },
    "mfaz" = {
      if(!is.null(.by)) {
        df <- df |>
          summarise(
            gam = apply_probit_method(.data$mfaz, .status = "gam"),
            sam = apply_probit_method(.data$mfaz, .status = "sam"),
            mam = .data$gam - .data$sam,
            .by = !!.by
          ) |>
          mutate(
            gam_p = .data$gam,
            sam_p = .data$sam,
            mam_p = .data$mam,
            gam = NA,
            sam = NA,
            mam = NA
          ) |>
          select(!2:4) # Make it fit in structure of the returned df in the main function
      } else {
        df <- df |>
          summarise(
            gam = apply_probit_method(.data$mfaz, .status = "gam"),
            sam = apply_probit_method(.data$mfaz, .status = "sam"),
            mam = .data$gam - .data$sam
          ) |>
          mutate(
            gam_p = .data$gam,
            sam_p = .data$sam,
            mam_p = .data$mam,
            gam = NA,
            sam = NA,
            mam = NA
          ) |>
          select(!2:4)## To make it fit in the structure of the df
      }
      df
    }
  )
}
