# Test check: process_wfhz_data() ----
testthat::test_that(
  "mw_wrangle_wfhz() works as designed",
  {

    df <- anthro.01 |>
      mw_wrangle_wfhz(
        sex = sex,
        weight = weight,
        height = height,
        .recode_sex = TRUE,
        .decimals = 3
      )

    ## Weight of wrong type/class ----
    df$w <- as.character(anthro.01$weight)
    ## Height of wrong type/class ----
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
