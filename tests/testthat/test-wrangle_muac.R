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

## Check if function errors when wrong input for sex is supplied ----
testthat::test_that(
  "mw_wrangle_muac() throws error when wrong sex input is supplied",
  {
    ### Sample data of sex code as "m" and "f" ----
    df <- data.frame(
      sex1 = c("m", "m", "m", "f", "f", "f", "f", "fe", "male", "female"),
      sex2 = c(1, 1, 1, 2, 2, 2, 2, 2, 3, 4),
      muac = seq(100, 300, 21),
      age = seq(6, 59, 5.8)
    )

    ### Tests ----
    testthat::expect_error(
      df |>
        mw_wrangle_age(
          dos = NULL,
          dob = NULL,
          age = age,
          .decimals = 3
        ) |>
        mw_wrangle_muac(
          sex = sex1,
          muac = muac,
          age = age,
          .recode_sex = TRUE,
          .recode_muac = TRUE,
          .to = "cm",
          .decimals = 3
      ),
      regexp = "Values for sex should either be 'm', 'f' or 1 and 2 for male and female respectively"
    )
  }
)

## Check if function errors when wrong input for sex is supplied ----
testthat::test_that(
  "mw_wrangle_muac() throws error when wrong sex input is supplied",
  {
    ### Sample data of sex code as "m" and "f" ----
    df <- data.frame(
      sex1 = c("m", "m", "m", "f", "f", "f", "f", "fe", "male", "female"),
      sex2 = c(1, 1, 1, 2, 2, 2, 2, 2, 3, 4),
      muac = seq(100, 300, 21),
      age = seq(6, 59, 5.8)
    )

    ### Tests ----
    testthat::expect_error(
      df |>
        mw_wrangle_age(
          dos = NULL,
          dob = NULL,
          age = age,
          .decimals = 3
        ) |>
        mw_wrangle_muac(
          sex = sex2,
          muac = muac,
          age = age,
          .recode_sex = FALSE,
          .recode_muac = TRUE,
          .to = "cm",
          .decimals = 3
        ),
      regexp = "Values for sex should either be 'm', 'f' or 1 and 2 for male and female respectively"
    )
  }
)

