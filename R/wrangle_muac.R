#'
#' @rdname wrangler
#'
#' @export
#'
mw_wrangle_muac <- function(df,
                            sex,
                            muac,
                            age = NULL,
                            .recode_sex = TRUE,
                            .recode_muac = TRUE,
                            .to = c("cm", "mm", "none"),
                            .decimals = 3) {

  ## Enforce options in argument .to ----
  .to <- match.arg(.to)

  ## Capture expressions to evaluate later ----
  recode_sex <- quote(
    if (.recode_sex) {
      sex <- ifelse({{ sex }} == "m", 1, 2)
    } else {{{ sex }}}
  )

  ## Capture expressions to evaluate later ----
  rec_muac <- quote(
    if (.recode_muac && .to == "cm") {
      muac <- recode_muac({{ muac }}, .to = "cm")
    } else if (.recode_muac && .to == "mm") {
      muac <- recode_muac({{ muac }}, .to = "mm")
    } else {{{ muac }}}
  )

  ## Difuse arguments to be evaluated later ----
  age <- rlang::eval_tidy(rlang::enquo(age), df)

  if (!is.null(age)) {
    ## Compute z-scores and identify flags on MFAZ ----
    df <- df |>
      mutate(
        muac = !!rec_muac,
        sex = !!recode_sex,
      ) |>
      addWGSR(
        sex = {{ "sex" }},
        firstPart = {{ "muac" }},
        secondPart = "age_days",
        index = "mfa",
        digits = .decimals
      ) |>
      mutate(
        flag_mfaz = do.call(flag_outliers, list(.data$mfaz, .from = "zscores"))
      )
  } else {
    ## Identify flags on the absolute MUAC values ----
    df <- df |>
      mutate(
        sex = !!recode_sex,
        flag_muac = do.call(flag_outliers, list({{ muac }}, .from = "absolute"))
      )
  }
  ## Return ----
  tibble::as_tibble(df)
}
