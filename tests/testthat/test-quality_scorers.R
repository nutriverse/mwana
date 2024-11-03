# Tests check: score_std_flags() ----
testthat::test_that(
  "Quality scorer works well",
  {
    ## Sample data ----
    std <-  c("Problematic", "Excellent", "Good", "Good", "Problematic",
    "Excellent", "Problematic", "Excellent", "Problematic", "Problematic",
    "Good", "Good", "Excellent", "Problematic", "Good")

    ## Expected results ----
    exp <- c(20, 0, 5, 5, 20, 0, 20, 0, 20, 20, 5, 5, 0, 20, 5)

    ## Observed results ----
    cl <- score_std_flags(std)

    ## Tests ----
    testthat::expect_vector(cl, ptype = numeric(), 15)
    testthat::expect_equal(cl, exp)
    testthat::expect_error(
      score_std_flags(as.numeric(exp)),
      regexp = paste0(
        "`x` must be of class `character` or `factor`; not ", shQuote(class(exp)), ". Please try again."
      )
    )
  }
)

# Test check: score_agesex_ratio() ----
testthat::test_that(
  "score_agesexr_dps() works as expected",
  {
    ## Sample data ----
    cl <- c(
      "Problematic", "Problematic", "Acceptable", "Acceptable", "Excellent",
      "Acceptable", "Acceptable", "Acceptable", "Acceptable", "Good",
      "Acceptable", "Acceptable", "Acceptable", "Problematic", "Good"
    )

    ## Expected results ----
    e <- c(10, 10, 4, 4, 0, 4, 4, 4, 4, 2, 4, 4, 4, 10, 2)

    ## Observed results ----
    o <- score_agesexr_dps(cl)

    ## Tests ----
    testthat::expect_vector(o, numeric(), 15)
    testthat::expect_equal(o, e)
    testthat::expect_error(
      score_agesexr_dps(e),
      regexp = paste0(
        "`x` must be of class `character` or `factor`; not ", shQuote(class(e)), ". Please try again."
        )
    )
  }
)

# Test check: score_skewkurt() ----
testthat::test_that(
  "score_skewkurt() works fine",
  {
    ## Sample data ----
    sk <- c(
      "Excellent", "Excellent", "Good", "Excellent", "Good",
      "Problematic", "Acceptable", "Excellent", "Problematic",
      "Good", "Problematic", "Problematic", "Problematic",
      "Excellent", "Problematic"
    )

    ## Expected results ----
    exp <- c(0, 0, 1, 0, 1, 5, 3, 0, 5, 1, 5, 5, 5, 0, 5)

    ## Observed results ----
    o <- score_skewkurt(sk)

    ## Tests ----
    testthat::expect_vector(o, numeric(), 15)
    testthat::expect_equal(o, exp)
    testthat::expect_error(
      score_skewkurt(exp),
      regexp = paste0(
        "`x` must be of class `character` or `factor`; not ", shQuote(class(exp)), ". Please try again."
      )
    )
  }
)


# Test check: score_overall_quality() ----
## With `.for` set to "mfaz" ----
testthat::test_that(
  "score_overall_quality() return the correct values for a given classification",
  {
    ## Sample data ----
    df <- data.frame(
      flagged_class = "Problematic",
      sd_class = "Good",
      skew_class = "Excellent",
      kurt_class = "Excellent",
      dps_class = "Acceptable",
      age_ratio_class = "Good",
      sex_ratio_class = "Problematic"
    )

    ## Expected results ----
    score <- 41

    ## Observed results ----
    obs <- score_overall_quality(
      cl_flags = df$flagged_class,
      cl_sex = df$sex_ratio_class,
      cl_age = df$age_ratio_class,
      cl_dps_m = df$dps_class,
      cl_std = df$sd_class,
      cl_skw = df$skew_class,
      cl_kurt = df$kurt_class,
      .for = "mfaz"
    )

    ## Tests ----
    testthat::expect_vector(object = obs, ptype = numeric(), size = 1)
    testthat::expect_equal(obs, score)
  }
)

## With `.for` set to "mfaz" ----
testthat::test_that(
  "score_overall_quality() return the correct values for a given classification",
  {
    ## Sample data ----
    df <- data.frame(
      flagged_class = "Good",
      sd_class = "Good",
      skew_class = "Excellent",
      kurt_class = "Excellent",
      age_ratio_class = "Good",
      sex_ratio_class = "Problematic",
      dps_h_class = "Excellent",
      dps_w_class = "Excellent"
    )

    ## Expected results ----
    score <- 22

    ## Observed results ----
    obs <- score_overall_quality(
      cl_flags = df$flagged_class,
      cl_sex = df$sex_ratio_class,
      cl_age = df$age_ratio_class,
      cl_dps_m = NULL,
      cl_std = df$sd_class,
      cl_skw = df$skew_class,
      cl_kurt = df$kurt_class,
      cl_dps_w = df$dps_h_class,
      cl_dps_h = df$dps_h_class,
      .for = "wfhz"
    )

    ## Tests ----
    testthat::expect_vector(object = obs, ptype = numeric(), size = 1)
    testthat::expect_equal(obs, score)
  }
)

