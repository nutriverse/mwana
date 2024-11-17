# Test check: mw_plausibility_check_wfhz() ----
testthat::test_that(
  "mw_plausibility_check_wfhz() works as expected",
  {
    ## Wrangle WFHZ data ----
    df <- anthro.01 |>
      mw_wrangle_age(
        dos = dos,
        dob = dob,
        age = age,
        .decimals = 2
      ) |>
      mw_wrangle_wfhz(
        sex = sex,
        weight = weight,
        height = height,
        .recode_sex = TRUE
      )

    ## Observed plausibility ----
    x <- df |>
      mw_plausibility_check_wfhz(
        sex = sex,
        age = age,
        weight = weight,
        height = height,
        flags = flag_wfhz
      )

    ## Tests ----
    testthat::expect_s3_class(x, "tbl_df")
    testthat::expect_vector(x)
    testthat::expect_equal(ncol(x), 19)
    testthat::expect_equal(nrow(x), 1)
    testthat::expect_true(
      all(c(
        "n", "flagged", "flagged_class", "sex_ratio",
        "sex_ratio_class", "age_ratio", "age_ratio_class",
        "dps_wgt", "dps_wgt_class", "dps_hgt", "dps_hgt_class",
        "sd", "sd_class", "skew", "skew_class", "kurt", "kurt_class",
        "quality_score", "quality_class"
      ) %in% names(x))
    )
  }
)

# Test check: mw_neat_output_wfhz() ----
testthat::test_that(
  "mw_neat_output_wfhz() works",
  {
    quality <- anthro.01 |>
      mw_wrangle_age(
        dos = dos,
        dob = dob,
        age = age,
        .decimals = 2
      ) |>
      mw_wrangle_wfhz(
        sex = sex,
        weight = weight,
        height = height,
        .recode_sex = TRUE
      ) |>
      mw_plausibility_check_wfhz(
        flags = flag_wfhz,
        sex = sex,
        age = age,
        weight = weight,
        height = height
      ) |>
      mw_neat_output_wfhz()

    ## Tests ----
    testthat::expect_s3_class(quality, "tbl_df")
    testthat::expect_equal(ncol(quality), 19)
    testthat::expect_equal(nrow(quality), 1)
    testthat::expect_true(
      all(c("Total children", "Flagged data (%)",
            "Class. of flagged data", "Sex ratio (p)", "Class. of sex ratio",
            "Age ratio (p)", "Class. of age ratio", "DPS weight (#)",
            "Class. DPS weight", "DPS height (#)", "Class. DPS height",
            "Standard Dev* (#)", "Class. of standard dev",
            "Skewness* (#)", "Class. of skewness", "Kurtosis* (#)",
            "Class. of kurtosis", "Overall score", "Overall quality"
      ) %in% names(quality)

      )
    )
  }
)

# Test check: mw_neat_output_wfhz() ----
testthat::test_that(
  "mw_neat_output_wfhz() works OK when `df` is grouped",
  {
    quality <- anthro.01 |>
      mw_wrangle_age(
        dos = dos,
        dob = dob,
        age = age,
        .decimals = 2
      ) |>
      mw_wrangle_wfhz(
        sex = sex,
        weight = weight,
        height = height,
        .recode_sex = TRUE
      ) |>
      group_by(area) |>
      mw_plausibility_check_wfhz(
        flags = flag_wfhz,
        sex = sex,
        age = age,
        weight = weight,
        height = height
      ) |>
      group_by(area) |>
      mw_neat_output_wfhz()

    ## Tests ----
    testthat::expect_s3_class(quality, "tbl_df")
    testthat::expect_equal(ncol(quality), 20)
    testthat::expect_equal(nrow(quality), 2)
    testthat::expect_true(
      all(c("Group", "Total children", "Flagged data (%)",
            "Class. of flagged data", "Sex ratio (p)", "Class. of sex ratio",
            "Age ratio (p)", "Class. of age ratio", "DPS weight (#)",
            "Class. DPS weight", "DPS height (#)", "Class. DPS height",
            "Standard Dev* (#)", "Class. of standard dev",
            "Skewness* (#)", "Class. of skewness", "Kurtosis* (#)",
            "Class. of kurtosis", "Overall score", "Overall quality"
      ) %in% names(quality)

      )
    )
  }
)

