# Test check: mw_plausibility_check_mfaz() ----
testthat::test_that(
  "mw_plausibility_check_mfaz() works as expected",
  {
    ## Wrangle age and MUAC data ----
    df <- anthro.01 |>
      mw_wrangle_age(
        dos = dos,
        dob = dob,
        age = age,
        .decimals = 2
      ) |>
      mw_wrangle_muac(
        sex = sex,
        muac = muac,
        age = "age",
        .recode_sex = TRUE,
        .recode_muac = TRUE,
        .to = "cm"
      )

    ## Observed results ----
    pl <- df |>
      mw_plausibility_check_mfaz(
        flags = flag_mfaz,
        sex = sex,
        muac = muac,
        age = age
      )

    ## Tests ----
    testthat::expect_s3_class(pl, "tbl_df")
    testthat::expect_vector(pl)
    testthat::expect_equal(ncol(pl), 17)
    testthat::expect_equal(nrow(pl), 1)
    testthat::expect_true(
      all(c(
        "n", "flagged", "flagged_class", "sex_ratio",
        "sex_ratio_class", "age_ratio", "age_ratio_class",
        "dps", "dps_class", "sd", "sd_class", "skew", "skew_class",
        "kurt", "kurt_class", "quality_score", "quality_class"
      ) %in% names(pl))
    )
  }
)

# Test check: mw_neat_output_mfaz() ----
testthat::test_that(
  "mw_neat_output_mfaz() works",
  {
    ## Workflow ----
    quality <- anthro.01 |>
      mw_wrangle_age(
        dos = dos,
        dob = dob,
        age = age,
        .decimals = 2
      ) |>
      mw_wrangle_muac(
        sex = sex,
        age = age,
        muac = muac,
        .recode_sex = TRUE,
        .recode_muac = TRUE,
        .to = "cm"
      ) |>
      mw_plausibility_check_mfaz(
        flags = flag_mfaz,
        sex = sex,
        muac = muac,
        age = age
      ) |>
      mw_neat_output_mfaz()

    ## Tests ----
    testthat::expect_s3_class(quality, "tbl_df")
    testthat::expect_equal(ncol(quality), 17)
    testthat::expect_equal(nrow(quality), 1)
    testthat::expect_true(
      all(c("Total children", "Flagged data (%)",
            "Class. of flagged data", "Sex ratio (p)", "Class. of sex ratio",
            "Age ratio (p)", "Class. of age ratio", "DPS (#)",
            "Class. of DPS", "Standard Dev* (#)", "Class. of standard dev",
            "Skewness* (#)", "Class. of skewness", "Kurtosis* (#)",
            "Class. of kurtosis", "Overall score", "Overall quality"
      ) %in% names(quality)

      )
    )
  }
)
