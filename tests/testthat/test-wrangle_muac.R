# Test check: mw_wrangle_muac() ----
## When age is available ----
testthat::test_that(
  "mw_wrangle_muac() works well when age is supplied",
  {

    ### Wrangle MUAC data ----
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

    ### Tests ----
    testthat::expect_true(is(df, "tbl"))
    testthat::expect_vector(df[["mfaz"]], size = 943)
    testthat::expect_true(is.double(df[["mfaz"]]))
    testthat::expect_vector(df[["flag_mfaz"]], size = 943)
    testthat::expect_true(is.numeric(df[["flag_mfaz"]]))
  }
)

# Test check: mw_wrangle_muac() ----
## When age = NULL ----
testthat::test_that(
  "mw_wrangle_muac() works well when age is not supplied",
  {

    ### Wrangle MUAC data ----
    df <- anthro.03 |>
      mw_wrangle_muac(
        sex = sex,
        muac = muac,
        age = NULL,
        .recode_sex = TRUE,
        .recode_muac = FALSE,
        .to = "none"
      )

    ### Tests ----
    testthat::expect_true(is(df, "tbl"))
    testthat::expect_vector(df[["flag_muac"]], size = 943)
    testthat::expect_true(is.numeric(df[["flag_muac"]]))
  }
)
