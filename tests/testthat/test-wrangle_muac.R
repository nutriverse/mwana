# Test check: mw_wrangle_muac() ----
testthat::test_that(
  "process_muac_data() works well",
  {

    df <- anthro.03 |>
      mw_wrangle_age(
        dos = NULL,
        dob = NULL,
        age = age,
        .decimals = 3
      ) |>
      mw_wrangle_muac(
        sex = sex,
        muac = muac,
        age = age,
        .recode_sex = TRUE,
        .recode_muac = TRUE,
        .to = "cm",
        .decimals = 3
      )

    ## Tests ----
    testthat::expect_true(is(df, "tbl"))
    testthat::expect_vector(df[["mfaz"]], size = 943)
    testthat::expect_true(is.double(df[["mfaz"]]))
    testthat::expect_vector(df[["flag_mfaz"]], size = 943)
    testthat::expect_true(is.numeric(df[["flag_mfaz"]]))
  }
)
