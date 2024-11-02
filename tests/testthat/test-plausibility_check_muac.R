## Test check: mw_plausibility_check_muac() ----

local(
  {
    ### Input data ----
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

    ### The test ----
    testthat::test_that(
      "check_plausibility_muac() return a df with expected lentgh and columns",
      {
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
  }
)

