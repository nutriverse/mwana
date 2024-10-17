#' Get a formatted and presentable output table for the plausibility checkers
#'
#' @description
#' Useful to getting the output returned from the plausibility checkers
#' into a presentable format. It converts scientific notation to standard
#' notations, round values and rename columns to meaningful names.
#'
#' @param df A summary table object of class `data.frame` returned by the
#' plausibility checkers.
#'
#' @returns A `data.frame` as `df`, columns columns renamed, values formatted and
#' ready to be shared.
#'
#' @examples
#'
#' ## Check the plausibility of WFHZ data ----
#'
#' anthro.01 |>
#' process_wfhz_data(
#' sex = sex,
#' weight = weight,
#' height = height,
#' .recode_sex = TRUE
#' ) |>
#' check_plausibility_wfhz(
#' sex = sex,
#' age = age,
#' weight = weight,
#' height = height,
#' flags = flag_wfhz,
#' area = area
#' ) |>
#' generate_pretty_table_wfhz()
#'
#' ## Check the plausibility of the absolute MUAC values ----
#'
#' anthro.01 |>
#' process_muac_data(
#' sex = sex,
#' muac = muac,
#' age = NULL,
#' .recode_sex = TRUE,
#' .recode_muac = FALSE,
#' unit = "none"
#' ) |>
#' check_plausibility_muac(
#' flags = flag_muac,
#' sex = sex,
#' muac = muac
#' ) |>
#' generate_pretty_table_muac()
#'
#' ## Check the plausibility of MFAZ data ----
#'
#' anthro.01 |>
#' process_age(
#' svdate = "dos",
#' birdate = "dob",
#' age = age
#' ) |>
#' process_muac_data(
#' sex = sex,
#' age = "age",
#' muac = muac,
#' .recode_sex = TRUE,
#' .recode_muac = TRUE,
#' unit = "cm"
#' ) |>
#' check_plausibility_mfaz(
#' flags = flag_mfaz,
#' sex = sex,
#' muac = muac,
#' age = age,
#' area = area
#' ) |>
#' generate_pretty_table_mfaz()
#'
#' @rdname pretty_table
#'
#' @export
#'
#'
generate_pretty_table_mfaz <- function(df) {

  ## Format data frame ----
  df <- df |>
    mutate(
      flagged = .data$flagged |>
        label_percent(accuracy = 0.1, suffix = "%", decimal.mark = ".")(),
      sex_ratio = .data$sex_ratio |>
        label_pvalue()(),
      age_ratio = .data$age_ratio |>
        label_pvalue()(),
      sd = round(.data$sd, digits = 2),
      dps = round(.data$dps),
      skew = round(.data$skew, digits = 2),
      kurt = round(.data$kurt, digits = 2)
    ) |>
    ## Rename columns ----
    setNames(
      c("Area", "Total children", "Flagged data (%)",
        "Class. of flagged data", "Sex ratio (p)", "Class. of sex ratio",
        "Age ratio (p)", "Class. of age ratio", "DPS (#)",
        "Class. of DPS", "Standard Dev* (#)", "Class. of standard dev",
        "Skewness* (#)", "Class. of skewness", "Kurtosis* (#)",
        "Class. of kurtosis", "Overall score", "Overall quality"
      )
    )
  ## Return data frame ----
  df
}


#'
#'
#' @rdname pretty_table
#'
#' @export
#'
#'
generate_pretty_table_wfhz <- function(df) {

  ## Format data frame ----
    df <- df |>
      mutate(
        flagged = .data$flagged |>
          label_percent(accuracy = 0.1, suffix = "%", decimal.mark = ".")(),
        sex_ratio = .data$sex_ratio |>
          label_pvalue()(),
        age_ratio = .data$age_ratio |>
          label_pvalue()(),
        sd = round(.data$sd, digits = 2),
        dps_wgt = round(.data$dps_wgt),
        dps_hgt = round(.data$dps_hgt),
        skew = round(.data$skew, digits = 2),
        kurt = round(.data$kurt, digits = 2)
      ) |>
      ## Rename columns ----
    setNames(
      c("Area", "Total children", "Flagged data (%)", "Class. of flagged data",
        "Sex ratio (p)", "Class. of sex ratio", "Age ratio (p)",
        "Class. of age ratio", "DPS weight (#)", "Class. DPS weight",
        "DPS height (#)", "Class. DPS height", "Standard Dev* (#)",
        "Class. of standard dev", "Skewness* (#)", "Class. of skewness",
        "Kurtosis* (#)", "Class. of kurtosis", "Overall score", "Overall quality"
      )
    )
  ## Return data frame ----
  df
}


#'
#'
#' @rdname pretty_table
#'
#' @export
#'
generate_pretty_table_muac <- function(df) {

  ## Format data frame ----
    df <- df |>
      mutate(
        flagged = .data$flagged |>
          label_percent(accuracy = 0.1, suffix = "%", decimal.mark = ".")(),
        sex_ratio = .data$sex_ratio  |>  scales::label_pvalue()(),
        sd = round(.data$sd, digits = 2),
        dps = round(.data$dps)
      ) |>
      ## Rename columns ----
    setNames(
      c("Total children", "Flagged data (%)", "Class. of flagged data", "Sex ratio (p)",
        "Class. of sex ratio", "DPS(#)", "Class. of DPS", "Standard Dev* (#)",
        "Class. of standard dev")
    )
    ## Return data frame ----
    df
}
