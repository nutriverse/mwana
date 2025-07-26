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
  switch(.status,
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
                                       .for = c("wfhz", "mfaz"),
                                       ...) {
  ## Defuse argument ----
  .by <- rlang::enquos(...)

  ## Enforce options in `.for` ----
  .for <- match.arg(.for)

  ## Apply groups if needed ----
  if (length(.by) > 0) df <- dplyr::group_by(df, !!!.by)
  
  ## Calculate probit-based prevalence ----
  var <- rlang::sym(.for)
  df <- df |>
    summarise(
      gam = apply_probit_method(!!var, .status = "gam"),
      sam = apply_probit_method(!!var, .status = "sam"),
      mam = .data$gam - .data$sam,
      .groups = "drop"
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
  df
}
