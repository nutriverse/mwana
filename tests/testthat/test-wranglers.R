# Test check: process_wfhz_data() ----
testthat::test_that(
  "mw_wrangle_wfhz() works as designed",
  {

    df <- anthro.01 |>
      mw_wrangle_wfhz(
        sex = sex,
        weight = "weight",
        height = "height",
        .recode_sex = TRUE
      )


    ## Tests ----
    testthat::expect_true(is(df, "tbl"))
    testthat::expect_vector(df[["wfhz"]], size = 1191)
    testthat::expect_true(is.numeric(df[["wfhz"]]))
    testthat::expect_vector(df[["flag_wfhz"]], size = 1191)
    testthat::expect_true(is.numeric(df[["flag_wfhz"]]))
  }
)

### Test check: process_muac_data() ----

local(
  {
    #### Sample data ----
    df <- data.frame(
      sex = c(2, 2, 1, 2, 1, 1, 1, 2, 2, 2, 2, 2, 2, 1, 1),
      muac = c(165, 222, 176, 150, 219, 193, 196, 203, 203,
               129, 97, 158, 156, 215, 214),
      age = c(13, 56, 53, 23, 43, 55, 25,16, 44, 19, 45, 36, 11, 31,26)
    )

    #### Expected results ----
    expected_results <- c(
      1.757, 3.059, 0.902, 0.161, 3.786, 1.892, 3.249, 4.217,
      2.651, -1.484, -5.529, 0.117, 1.151, 4.151, 4.415
    )

    #### Observed results ----
    df <- df |>
      mw_wrangle_age(
        dos = NULL,
        dob = NULL,
        age = age,
        .decimals = 3
      ) |>
      mw_wrangle_muac(
        sex = sex,
        muac = muac,
        age = "age",
        .recode_sex = FALSE,
        .recode_muac = TRUE,
        .to = "cm"
      )

    #### The Test ----
    testthat::test_that(
      "process_muac_data() works well",
      {
        testthat::expect_vector(df[["mfaz"]], size = 15)
        testthat::expect_vector(df[["flag_mfaz"]], size = 15)
        testthat::expect_equal(df[["mfaz"]], expected_results)
      }
    )
  }
)

