# Test checks: Classifiers------------------------------------------------------

## Test check: classify_percent_flagged() ----
### Test check: classify_percent_flagged() with method set to "mfaz" ----

local(
  {
    #### Sample data ----
    props <- c(
      0.0, 0.0, 0.01, 0.015, 0.2,
      0.015, 0.016, 0.017, 0.05, 0.06,
      0.03, 0.03, 0.04, 0.000001, 0
    )

    #### Expected results ----
    expected_results <- c(
      "Excellent", "Excellent", "Excellent", "Good", "Problematic",
      "Good", "Acceptable", "Acceptable", "Problematic", "Problematic",
      "Problematic", "Problematic", "Problematic", "Excellent", "Excellent"
    ) |>
      factor(levels = c("Excellent", "Good", "Acceptable", "Problematic"))

    #### Observed results ----
    class_flagged_data <- classify_percent_flagged(props, type = "mfaz")

    #### The test ----
    testthat::test_that(
      "classify_percent_flagged() with type set to 'mfaz' returns
      expected output and correct classifications",
      {
        testthat::expect_vector(
          class_flagged_data,
          ptype = factor(
            c("Excellent", "Good", "Acceptable", "Problematic"),
            levels = c("Excellent", "Good", "Acceptable", "Problematic")
          ),
          size = 15
        )
        testthat::expect_equal(class_flagged_data, expected_results)
      }
    )
  }
)

### Test check: classify_percent_of_outliers() with method set to "crude" ----

local(
  {
    #### Sample data ----
    props <- c(
      0.0, 0.0, 0.01, 0.015, 0.2,
      0.015, 0.016, 0.017, 0.05, 0.06,
      0.03, 0.03, 0.04, 0.000001, 0
    )

    #### Expected results ----
    expected_results <- c(
      "Excellent", "Excellent", "Excellent", "Good", "Problematic",
      "Good", "Acceptable", "Acceptable", "Problematic", "Problematic",
      "Problematic", "Problematic", "Problematic", "Excellent", "Excellent"
    ) |>
      factor(levels = c("Excellent", "Good", "Acceptable", "Problematic"))

    #### Observed results ----
    class_flagged_data <- classify_percent_flagged(props, type = "crude")

    #### The test ----
    testthat::test_that(
      "classify_percent_flagged() with type set to 'crude' returns
      expected output and correct classifications",
      {
        testthat::expect_vector(
          class_flagged_data,
          ptype = factor(
            c("Excellent", "Good", "Acceptable", "Problematic"),
            levels = c("Excellent", "Good", "Acceptable", "Problematic")
          ),
          size = 15
        )
        testthat::expect_equal(class_flagged_data, expected_results)
      }
    )
  }
)

## Test check: classify_age_sex_ratio() ----

local(
  {
    ### Sample data ----
    pvalues <- c(
      0, 0, 0.01, 0.011, 0.2,
      0.015, 0.016, 0.017, 0.05, 0.06,
      0.03, 0.03, 0.04, 0.000001, 0.07
    )

    ### Expected results ----
    expected_results <- c(
      "Problematic", "Problematic", "Acceptable", "Acceptable", "Excellent",
      "Acceptable", "Acceptable", "Acceptable", "Acceptable", "Good",
      "Acceptable", "Acceptable", "Acceptable", "Problematic", "Good"
    )

    ### Observed results ----
    class_age_sex_ratio <- classify_age_sex_ratio(pvalues)

    ### The test ----
    testthat::test_that(
      "classify_age_sex_ratio returns expected outpout and correct
      classifications",
      {
        testthat::expect_vector(
          class_age_sex_ratio,
          ptype = character(),
          size = 15
        )
        testthat::expect_equal(class_age_sex_ratio, expected_results)
      }
    )
  }
)

## Test check: classify_sd() ----
### Test check: classify_sd() with method set to "zscore" ----
local(
  {
    #### Sample data ----
    sdvalues <- c(
      1.253, 1.037, 0.876, 0.861, 0.8,
      1.083, 1.5, 0.922, 1.269, 0.797,
      0.880, 0.853, 1.041, 1.247, 0.9
    )

    #### Expected results ----
    expected_results <- c(
      "Problematic", "Excellent", "Good", "Good", "Problematic",
      "Excellent", "Problematic", "Excellent", "Problematic", "Problematic",
      "Good", "Good", "Excellent", "Problematic", "Good"
    )

    #### Observed results ----
    class_sd_mfaz <- classify_sd(sdvalues, type = "zscore")

    #### The test ----
    testthat::test_that(
      "classify_sd with method set to 'zscore' returns expected outpout
      and correct classifications",
      {
        testthat::expect_vector(
          class_sd_mfaz,
          ptype = character(),
          size = 15
        )
        testthat::expect_equal(class_sd_mfaz, expected_results)
      }
    )
  }
)

### Test check: classify_sd() with method set to "crude" ----

local(
  {
    #### Sample data ----
    sdvalues <- c(12, 12, 13, 11, 13, 17, 14, 11, 16, 13, 15, 17, 17, 11, 20)

    #### Expected results ----
    expected_results <- c(
      "Excellent", "Excellent", "Acceptable", "Excellent", "Acceptable",
      "Problematic", "Poor", "Excellent", "Problematic", "Acceptable",
      "Problematic", "Problematic", "Problematic", "Excellent", "Problematic"
    ) |>
      factor(levels = c("Excellent", "Acceptable", "Poor", "Problematic"))

    #### Observed results ----
    class_sd_muac <- classify_sd(sdvalues, type = "crude")

    #### The test ----
    testthat::test_that(
      "classify_sd() returns expected outpout and correct classifications",
      {
        testthat::expect_vector(
          class_sd_muac,
          ptype = factor(
            c("Excellent", "Acceptable", "Poor", "Problematic"),
            levels = c("Excellent", "Acceptable", "Poor", "Problematic")
          ),
          size = 15
        )
        testthat::expect_equal(class_sd_muac, expected_results)
      }
    )
  }
)

### Test check: classify_overall_quality() ----

local(
  {
    #### Sample data ----
    df <- data.frame(quality_score = 29)

    #### Expected result ----
    expected_r <- dplyr::tibble(quality_class = "Problematic")

    #### Observed results ----
    obs <- dplyr::tibble(classify_overall_quality(df))

    #### The test ----
    testthat::test_that(
      "classify_overall_quality() works",
      {
        testthat::expect_s3_class(obs, "tbl")
        testthat::expect_equal(names(obs[[1]]), names(expected_r[[1]]))
        testthat::expect_equal(obs[[1]] == "Problematic", expected_r[[1]] == "Problematic")
      }
    )
  }
)
