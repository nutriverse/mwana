# Test check: mw_plausibility_check_muac() ----
testthat::test_that(
  "mw_plausibility_check_muac() return a df with expected lentgh and columns",
  {
    ## Workflow ----
    df <- anthro.01 |>
      mw_wrangle_muac(
        sex = sex,
        muac = muac,
        age = NULL,
        .recode_sex = TRUE,
        .recode_muac = FALSE,
        .to = "none"
      ) |>
      mw_plausibility_check_muac(
        sex = sex,
        muac = muac,
        flags = flag_muac
      )

    ## Tests ----
    testthat::expect_s3_class(df, "data.frame")
    testthat::expect_vector(df)
    testthat::expect_equal(ncol(df), 9)
    testthat::expect_equal(nrow(df), 1)
    testthat::expect_true(
      all(c(
        "n", "flagged", "flagged_class", "sex_ratio",
        "sex_ratio_class", "dps", "dps_class",
        "sd", "sd_class"
      ) %in% names(df)

      )
    )
  }
)

# Test check: mw_neat_output_muac()----
testthat::test_that(
  "mw_neat_output_muac() works",
  {
    ## Workflow ----
    quality <- anthro.01 |>
      mw_wrangle_muac(
        sex = sex,
        muac = muac,
        .recode_sex = TRUE,
        .recode_muac = FALSE,
        .to = "none"
      ) |>
      mw_plausibility_check_muac(
        flags = flag_muac,
        sex = sex,
        muac = muac
      )|>
      mw_neat_output_muac()

    ## Tests ----
    testthat::expect_s3_class(quality, "data.frame")
    testthat::expect_equal(ncol(quality), 9)
    testthat::expect_equal(nrow(quality), 1)
    testthat::expect_true(
      all(c(
        "Total children", "Flagged data (%)", "Class. of flagged data",
        "Sex ratio (p)", "Class. of sex ratio", "DPS(#)", "Class. of DPS",
        "Standard Dev* (#)", "Class. of standard dev"
      ) %in% names(quality)
      )
    )

  }
)

# Test check: mw_neat_output_muac()----
testthat::test_that(
  "mw_neat_output_muac() works when `df` is grouped",
  {
    ## Workflow ----
    quality <- anthro.01 |>
      mw_wrangle_muac(
        sex = sex,
        muac = muac,
        .recode_sex = TRUE,
        .recode_muac = FALSE,
        .to = "none"
      ) |>
      mw_plausibility_check_muac(
        flags = flag_muac,
        sex = sex,
        muac = muac, 
        .by = area
      )|>
      mw_neat_output_muac(.by = area)

    ## Tests ----
    testthat::expect_s3_class(quality, "data.frame")
    testthat::expect_equal(ncol(quality), 10)
    testthat::expect_equal(nrow(quality), 2)
    testthat::expect_true(
      all(c(
        "Group","Total children", "Flagged data (%)", "Class. of flagged data",
        "Sex ratio (p)", "Class. of sex ratio", "DPS(#)", "Class. of DPS",
        "Standard Dev* (#)", "Class. of standard dev"
      ) %in% names(quality)
      )
    )

  }
)

