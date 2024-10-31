# Test check: mw_wrangle_wfhz() ----
testthat::test_that(
  "mw_wrangle_wfhz() works as designed",
  {

    ## Wrangle WFHZ data ---
    df <- anthro.01 |>
      mw_wrangle_wfhz(
        sex = sex,
        weight = weight,
        height = height,
        .recode_sex = TRUE,
        .decimals = 3
      )

    ## Weight of a wrong class ----
    df$w <- as.character(anthro.01$weight)
    ## Height of a wrong class ----
    df$h <- as.character(anthro.01$height)


    ## Tests ----
    testthat::expect_true(is(df, "tbl"))
    testthat::expect_vector(df[["wfhz"]], size = 1191)
    testthat::expect_true(is.double(df[["wfhz"]]))
    testthat::expect_vector(df[["flag_wfhz"]], size = 1191)
    testthat::expect_true(is.numeric(df[["flag_wfhz"]]))
    testthat::expect_error(mw_wrangle_wfhz(df, sex, w, height, TRUE))
    testthat::expect_error(mw_wrangle_wfhz(df, sex, weight, h, TRUE))
  }
)

## Check if function errors when wrong input for sex is supplied ----
testthat::test_that(
  "mw_wrangle_wfhz() throws error when a wrong input is supplied",
  {
    ### Sample data of sex code as "m" and "f" ----
    df <- data.frame(
      sex1 = c("m", "m", "m", "f", "f", "f", "f", "fe", "male", "female", "f"),
      sex2 = c(1, 1, 1, 2, 2, 2, 2, 2, 3, 4, 1),
      ht = seq(70, 100, 2.9),
      wt = seq(6, 14, 0.8)
    )

    ### Tests ----
    testthat::expect_error(
      df |>
        mw_wrangle_wfhz(
          sex = sex1,
          weight = wt,
          height = ht,
          .recode_sex = TRUE
        ),
      regexp = "Values for sex should either be 'm', 'f' or 1 and 2 for male and female respectively"
    )
  }
)

## Check if function errors when wrong input for sex is supplied ----
testthat::test_that(
  "mw_wrangle_wfhz() throws error when a wrong input is supplied",
  {
    ### Sample data of sex code as "m" and "f" ----
    df <- data.frame(
      sex1 = c("m", "m", "m", "f", "f", "f", "f", "fe", "male", "female", "f"),
      sex2 = c(1, 1, 1, 2, 2, 2, 2, 2, 3, 4, 1),
      ht = seq(70, 100, 2.9),
      wt = seq(6, 14, 0.8)
    )

    ### Tests ----
    testthat::expect_error(
      df |>
        mw_wrangle_wfhz(
          sex = sex2,
          weight = wt,
          height = ht,
          .recode_sex = TRUE
        ),
      regexp = "Values for sex should either be 'm', 'f' or 1 and 2 for male and female respectively"
    )
  }
)
