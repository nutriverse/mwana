# Test check for input data processors -----------------------------------------

## Test check: "flag_outliers()" ----
### Test check: flag_outliers with 'type' set to "zscore" ----

local(
  {
    #### Sample data ----
    mfaz <- seq(-0.6, 0.9, by = 0.003) |>
      sample(size = 50, replace = TRUE)

    #### Mean of mfaz ----
    mean_mfaz <- mean(mfaz)

    #### Expected results ----
    expected_results <- ifelse(
      mfaz < (mean_mfaz - 3) | mfaz > (mean_mfaz + 3), 1, 0
    )

    #### Observed results ----
    flags_mfaz <- flag_outliers(mfaz, type = "zscore")

    #### The test ----
    testthat::test_that(
      "flag_outliers() returns expected output",
      {
        testthat::expect_vector(flags_mfaz, size = 50)
        testthat::expect_equal(flags_mfaz, expected_results)
      }
    )
  }
)

### Test check: flag_outliers() with 'type' set to "crude" ----
local(
  {
    #### Sample data ----
    crude <- seq(80, 270, by = 4) |>
      sample(size = 20, replace = TRUE)

    #### Expected results ----
    expected_results <- ifelse(crude < 100 | crude > 200, 1, 0)

    #### Observed results ----
    flags_crude <- flag_outliers(crude, type = "crude")

    #### The test ----
    testthat::test_that(
      "flag_outliers with 'type' set for 'crude' return the correct output",
      {
        testthat::expect_vector(flags_crude, size = 20)
        testthat::expect_equal(flags_crude, expected_results)
      }
    )
  }
)

## Test check: remove_flags() -----
### Test check: remove_flags() with method set to "mfaz"----
local({
  #### Observed results ----
  processed_df <- anthro.01 |>
    mw_wrangle_age(
      dos = dos,
      dob = dob,
      age = age
    ) |>
    process_muac_data(
      sex = sex,
      muac = muac,
      age = "age",
      .recode_sex = TRUE,
      .recode_muac = TRUE,
      unit = "cm"
    )

  processed_df[["not_flag"]] <- remove_flags(processed_df[["mfaz"]], unit = "zscore")

  #### The test ----
  testthat::test_that(
    "remove_flags() assign NA's when flaggs are identified",
    {
      with(
        processed_df,
        testthat::expect_length(processed_df[["not_flag"]], 1191)
      )
    }
  )
})

### Test check: remove_flags() with method set to "crude"----
local({
  #### Observed results ----
  processed_df <- anthro.01 |>
    mw_wrangle_age(
      dos = dos,
      dob = dob,
      age = age
    ) |>
    process_muac_data(
      sex = sex,
      muac = muac,
      age = NULL,
      .recode_sex = TRUE,
      .recode_muac = FALSE,
      unit = "none"
    )
  processed_df[["not_flag"]] <- remove_flags(processed_df[["muac"]], unit = "crude")

  #### The test ----
  testthat::test_that(
    "remove_flags() assign NA's when flaggs are identified",
    {
      with(
        processed_df,
        testthat::expect_length(processed_df[["not_flag"]], 1191)
      )
    }
  )
})

### Test check: recode_muac() ----
local(
  {
    #### Sample data ----
    muac <- seq(90, 250, by = 4)

    #### Expected results ----
    expected_results <- muac / 10

    ### Observed results ----
    muac_cm <- recode_muac(muac, unit = "cm")

    #### The test ----
    testthat::test_that(
      "recode_muac() works well",
      {
        testthat::expect_vector(muac_cm, size = 41)
        testthat::expect_equal(muac_cm, expected_results)
      }
    )
  }
)

### Test check: recode_muac() ----
local(
  {
    #### Sample data ----
    muac <- seq(9.0, 25.0, by = 0.2)

    #### Expected results ----
    expected_results <- muac * 10

    ### Observed results ----
    muac_mm <- recode_muac(muac, unit = "mm")

    #### The test ----
    testthat::test_that(
      "recode_muac() works well",
      {
        testthat::expect_vector(muac_mm, size = 81)
        testthat::expect_equal(muac_mm, expected_results)
      }
    )
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
      1.757, 3.057, 0.902, 0.161, 3.786, 1.892, 3.249, 4.217,
      2.651, -1.484, -5.529, 0.117, 1.151, 4.151, 4.415
    )

    #### Observed results ----
    df <- df |>
      mw_wrangle_age(
        dos = NULL,
        dob = NULL,
        age = age
      ) |>
      process_muac_data(
        sex = sex,
        muac = muac,
        age = "age",
        .recode_sex = FALSE,
        .recode_muac = TRUE,
        unit = "cm"
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

### Test check: process_wfhz_data() ----
local(
  {
    df <- anthro.01 |>
      process_wfhz_data(
        sex = sex,
        weight = weight,
        height = height,
        .recode_sex = TRUE
      )

    #### The Test ----
    testthat::test_that(
      "process_wfhz_data() works as designed",
      {
        testthat::expect_vector(df[["wfhz"]], size = 1191)
        testthat::expect_vector(df[["flag_wfhz"]], size = 1191)
        testthat::expect_vector(is.numeric(df[["mfaz"]]))
      }
    )

  }
)
