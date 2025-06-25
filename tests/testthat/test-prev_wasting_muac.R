# Test check: set_analysis_path() ----
testthat::test_that(
  "set_analysis_path() works",
  {
    ## Input data ----
    age_ratio_class_1 <- "Problematic"
    age_ratio_class_2 <- "Good"
    std_class_1 <- "Excellent"
    std_class_2 <- "Problematic"

    ## Expected results ----
    expected_1 <- "weighted"
    expected_2 <- "missing"
    expected_3 <- "unweighted"

    ## Observed results ----
    obs_1 <- set_analysis_path(age_ratio_class_1, std_class_1)
    obs_2 <- set_analysis_path(age_ratio_class_1, std_class_2)
    obs_3 <- set_analysis_path(age_ratio_class_2, std_class_1)

    ## Tests ----
    testthat::expect_equal(obs_1, expected_1)
    testthat::expect_equal(obs_2, expected_2)
    testthat::expect_equal(obs_3, expected_3)
  }
)

# Test check: smart_tool_case_definition () with edema set o NULL ----
testthat::test_that(
  "smart_tool_case_definition() does its job well",
  {
    ## Input data ----
    muac_values <- c(
      123, 129, 126, 113, 130, 122, 112, 124, 128,
      121, 120, 110, 114, 125, 119, 127, 117, 118, 111, 115
    )

    ## Expected results ----
    expected <- c(
      "mam", "not wasted", "not wasted", "sam", "not wasted", "mam", "sam", "mam",
      "not wasted", "mam", "mam", "sam", "sam", "not wasted", "mam", "not wasted",
      "mam", "mam", "sam", "mam"
    )

    ## Observed results ----
    obs <- smart_tool_case_definition(muac = muac_values, edema = NULL)

    ## Tests ----
    testthat::expect_vector(obs, ptype = "character", size = 20)
    testthat::expect_equal(obs, expected)
  }
)

# Test check: smart_tool_case_definition() with edema supplied ----
testthat::test_that(
  "smart_tool_case_definition() does its job well",
  {
    ## Input data ----
    muac_values <- c(
      123, 129, 126, 113, 130, 122, 112, 124, 128,
      121, 120, 110, 114, 125, 119, 127, 117, 118, 111, 115
    )
    edema <- c(
      "n", "n", "n", "n", "y", "y", "y", "n", "y", "n", "n", "y", "n",
      "y", "n", "n", "n", "n", "n", "n"
    )

    ## Expected results ----
    expected <- c(
      "mam", "not wasted", "not wasted", "sam", "sam", "sam", "sam", "mam",
      "sam", "mam", "mam", "sam", "sam", "sam", "mam", "not wasted",
      "mam", "mam", "sam", "mam"
    )

    ## Observed results ----
    obs <- smart_tool_case_definition(muac = muac_values, edema = edema)

    ## Tests ----
    testthat::expect_vector(obs, ptype = "character", size = 20)
    testthat::expect_equal(obs, expected)
  }
)

# Test check: smart_age_weighting() ----
## Edema set to !NULL ----
testthat::test_that(
  "smart_age_weighting() works amazing",
  {
    ### Input data ----
    x <- mfaz.01 |>
      mw_wrangle_age(
        age = age
      ) |>
      mw_wrangle_muac(
        sex = sex,
        muac = muac,
        age = age,
        .recode_sex = TRUE,
        .recode_muac = TRUE,
        .to = "cm"
      ) |>
      subset(flag_mfaz == 0) |>
      mutate(muac = recode_muac(muac, .to = "mm"))


    #### Expected results calculated in the CDC/SMART MUAC tool ----
    expect_sam <- 0.021
    expect_mam <- 0.081

    #### Observed results ----
    obs_sam <- with(
      x,
      smart_age_weighting(
        muac = muac,
        edema = edema,
        age = age,
        .form = "sam"
      )
    )
    obs_mam <- with(
      x,
      smart_age_weighting(
        muac = muac,
        edema = edema,
        age = age,
        .form = "mam"
      )
    )

    ## Tests ----
    testthat::expect_vector(obs_sam, size = 1)
    testthat::expect_vector(obs_mam, size = 1)
    testthat::expect_equal(round(obs_sam, 3), expect_sam)
    testthat::expect_equal(round(obs_mam, 3), expect_mam)
  }
)

## Edema set to NULL ----
testthat::test_that(
  "smart_age_weighting() works amazing",
  {
    ## Input data ----
    x <- mfaz.01 |>
      mw_wrangle_age(
        age = age,
        .decimals = 2
      ) |>
      mw_wrangle_muac(
        sex = sex,
        muac = muac,
        age = age,
        .recode_sex = TRUE,
        .recode_muac = TRUE,
        .to = "cm"
      ) |>
      subset(flag_mfaz == 0) |>
      mutate(
        muac = recode_muac(muac, .to = "mm")
      )


    #### Expected results calculated in the CDC/SMART MUAC tool ----
    expect_sam <- 0.014
    expect_mam <- 0.080

    #### Observed results ----
    obs_sam <- with(
      x,
      smart_age_weighting(
        muac = muac,
        age = age,
        .form = "sam"
      )
    )
    obs_mam <- with(
      x,
      smart_age_weighting(
        muac = muac,
        age = age,
        .form = "mam"
      )
    )

    ## Tests ----
    testthat::expect_vector(obs_sam, size = 1)
    testthat::expect_vector(obs_mam, size = 1)
    testthat::expect_equal(round(obs_sam, 3), expect_sam)
    testthat::expect_equal(round(obs_mam, 2), expect_mam)
  }
)

# Test check: mw_estimate_prevalence_muac() ----
## When age_ratio & std != problematic & !is.null(wt) & !is.null(edema) ----
testthat::test_that(
  "mw_estimate_prevalence_muac() yields correct estimates when edema and survey
    weights are supplied",
  {
    ### Get the prevalence estimates ----
    p <- anthro.02 |>
      mw_estimate_prevalence_muac(edema = edema, wt = wtfactor, .by = NULL)

    ### Expected results ----
    ### GAM estimates and uncertainty ----
    n_gam <- 118
    p_gam <- 5.6
    p_gam_lci <- 4.3
    p_gam_uci <- 6.9
    deff <- 1.86

    ### SAM estimates and uncertainty ----
    n_sam <- 29
    p_sam <- 1.7
    p_sam_lci <- 0.9
    p_sam_uci <- 2.4

    ### MAM estimates and uncertainty ----
    n_mam <- 89
    p_mam <- 4.0
    p_mam_lci <- 3.0
    p_mam_uci <- 4.9

    ### Tests ----

    testthat::expect_equal(p[[1]][1], n_gam)
    testthat::expect_equal(round(p[[2]][1] * 100, 1), p_gam)
    testthat::expect_equal(round(p[[3]][1] * 100, 1), p_gam_lci)
    testthat::expect_equal(round(p[[4]][1] * 100, 1), p_gam_uci)
    testthat::expect_equal(p[[6]][1], n_sam)
    testthat::expect_equal(round(p[[7]][1] * 100, 1), p_sam)
    testthat::expect_equal(round(p[[8]][1] * 100, 1), p_sam_lci)
    testthat::expect_equal(round(p[[9]][1] * 100, 1), p_sam_uci)
    testthat::expect_equal(p[[11]][1], n_mam)
    testthat::expect_equal(round(p[[12]][1] * 100, 1), p_mam)
    testthat::expect_equal(round(p[[13]][1] * 100, 1), p_mam_lci)
    testthat::expect_equal(round(p[[14]][1] * 100, 1), p_mam_uci)
  }
)

## When age_ratio & std != problematic & is.null(wt) & !is.null(edema) ----
testthat::test_that(
  "mw_estimate_prevalence_muac() yields correct estimates survey
    weights are not supplied",
  {
    ### Get the prevalence estimates ----
    p <- anthro.02 |>
      mw_estimate_prevalence_muac(wt = NULL)

    ### Expected results ----
    ### GAM estimates and uncertainty ----
    n_gam <- 106
    p_gam <- 4.9
    p_gam_lci <- 3.8
    p_gam_uci <- 5.9

    ### SAM estimates and uncertainty ----
    n_sam <- 16
    p_sam <- 0.7
    p_sam_lci <- 0.4
    p_sam_uci <- 1.1

    ### MAM estimates and uncertainty ----
    n_mam <- 90
    p_mam <- 4.1
    p_mam_lci <- 3.2
    p_mam_uci <- 5.1

    ### Tests ----

    testthat::expect_equal(p[[1]][1], n_gam)
    testthat::expect_equal(round(p[[2]][1] * 100, 1), p_gam)
    testthat::expect_equal(round(p[[3]][1] * 100, 1), p_gam_lci)
    testthat::expect_equal(round(p[[4]][1] * 100, 1), p_gam_uci)
    testthat::expect_equal(p[[6]][1], n_sam)
    testthat::expect_equal(round(p[[7]][1] * 100, 1), p_sam)
    testthat::expect_equal(round(p[[8]][1] * 100, 1), p_sam_lci)
    testthat::expect_equal(round(p[[9]][1] * 100, 1), p_sam_uci)
    testthat::expect_equal(p[[11]][1], n_mam)
    testthat::expect_equal(round(p[[12]][1] * 100, 1), p_mam)
    testthat::expect_equal(round(p[[13]][1] * 100, 1), p_mam_lci)
    testthat::expect_equal(round(p[[14]][1] * 100, 1), p_mam_uci)
  }
)

## When age_ratio & std != problematic & !is.null(wt) & !is.null(edema) ----
testthat::test_that(
  "mw_estimate_prevalence_muac() yields correct estimates when edema is not
    supplied",
  {
    ### Get the prevalence estimates ----
    p <- anthro.02 |>
      mw_estimate_prevalence_muac(edema = NULL, wt = wtfactor, .by = NULL)

    ### Expected results ----
    #### GAM estimates and uncertainty ----
    n_gam <- 106
    p_gam <- 5.0
    p_gam_lci <- 3.8
    p_gam_uci <- 6.2
    deff <- 1.75

    #### SAM estimates and uncertainty ----
    n_sam <- 16
    p_sam <- 0.9
    p_sam_lci <- 0.4
    p_sam_uci <- 1.5

    #### MAM estimates and uncertainty ----
    n_mam <- 90
    p_mam <- 4.0
    p_mam_lci <- 3.1
    p_mam_uci <- 5.0

    ### Tests ----
    testthat::expect_equal(p[[1]][1], n_gam)
    testthat::expect_equal(round(p[[2]][1] * 100, 1), p_gam)
    testthat::expect_equal(round(p[[3]][1] * 100, 1), p_gam_lci)
    testthat::expect_equal(round(p[[4]][1] * 100, 1), p_gam_uci)
    testthat::expect_equal(p[[6]][1], n_sam)
    testthat::expect_equal(round(p[[7]][1] * 100, 1), p_sam)
    testthat::expect_equal(round(p[[8]][1] * 100, 1), p_sam_lci)
    testthat::expect_equal(round(p[[9]][1] * 100, 1), p_sam_uci)
    testthat::expect_equal(p[[11]][1], n_mam)
    testthat::expect_equal(round(p[[12]][1] * 100, 1), p_mam)
    testthat::expect_equal(round(p[[13]][1] * 100, 1), p_mam_lci)
    testthat::expect_equal(round(p[[14]][1] * 100, 1), p_mam_uci)
  }
)

## When age_ratio & std != problematic & is.null(wt) ----
testthat::test_that(
  "mw_estimate_prevalence_muac() yields correct estimates when edema is not supplied",
  {
    ### Get prevalence estimates ----
    p <- anthro.02 |>
      mw_estimate_prevalence_muac(edema = NULL, .by = NULL)

    ### Expected results ----
    #### GAM estimates and uncertainty ----
    n_gam <- 106
    p_gam <- 4.9
    p_gam_lci <- 3.8
    p_gam_uci <- 5.9

    #### SAM estimates and uncertainty ----
    n_sam <- 16
    p_sam <- 0.7
    p_sam_lci <- 0.4
    p_sam_uci <- 1.1

    #### MAM estimates and uncertainty ----
    n_mam <- 90
    p_mam <- 4.1
    p_mam_lci <- 3.2
    p_mam_uci <- 5.1

    ### The test ----
    testthat::expect_equal(p[[1]][1], n_gam)
    testthat::expect_equal(round(p[[2]][1] * 100, 1), p_gam)
    testthat::expect_equal(round(p[[3]][1] * 100, 1), p_gam_lci)
    testthat::expect_equal(round(p[[4]][1] * 100, 1), p_gam_uci)
    testthat::expect_equal(p[[6]][1], n_sam)
    testthat::expect_equal(round(p[[7]][1] * 100, 1), p_sam)
    testthat::expect_equal(round(p[[8]][1] * 100, 1), p_sam_lci)
    testthat::expect_equal(round(p[[9]][1] * 100, 1), p_sam_uci)
    testthat::expect_equal(p[[11]][1], n_mam)
    testthat::expect_equal(round(p[[12]][1] * 100, 1), p_mam)
    testthat::expect_equal(round(p[[13]][1] * 100, 1), p_mam_lci)
    testthat::expect_equal(round(p[[14]][1] * 100, 1), p_mam_uci)
  }
)


## When age_ratio & std != problematic & !is.null(wt) with .by = province
testthat::test_that(
  "mw_estimate_prevalence_muac() yields correct estimates when .by is
    used",
  {
    ### Get prevalence estimates ----
    p <- anthro.02 |>
      mw_estimate_prevalence_muac(
        edema = edema,
        wt = wtfactor,
        .by = province
      )

    ### Expected results for Zambezia province ----
    #### GAM estimates and uncertainty ----
    n_gam <- 57
    p_gam <- 5.5
    p_gam_lci <- 3.8
    p_gam_uci <- 7.2
    deff <- 1.67

    #### SAM estimates and uncertainty ----
    n_sam <- 10
    p_sam <- 1.3
    p_sam_lci <- 0.4
    p_sam_uci <- 2.2

    #### MAM estimates and uncertainty ----
    n_mam <- 47
    p_mam <- 4.2
    p_mam_lci <- 3.0
    p_mam_uci <- 5.4

    #### Sum of weigths ----
    sum_wt <- 880902

    ### The test ----
    testthat::expect_equal(p[[2]][2], n_gam)
    testthat::expect_equal(round(p[[3]][2] * 100, 1), p_gam)
    testthat::expect_equal(round(p[[4]][2] * 100, 1), p_gam_lci)
    testthat::expect_equal(round(p[[5]][2] * 100, 1), p_gam_uci)
    testthat::expect_equal(round(p[[6]][2], 2), deff)
    testthat::expect_equal(p[[7]][2], n_sam)
    testthat::expect_equal(round(p[[8]][2] * 100, 1), p_sam)
    testthat::expect_equal(round(p[[9]][2] * 100, 1), p_sam_lci)
    testthat::expect_equal(round(p[[10]][2] * 100, 1), p_sam_uci)
    testthat::expect_equal(p[[12]][2], n_mam)
    testthat::expect_equal(round(p[[13]][2] * 100, 1), p_mam)
    testthat::expect_equal(round(p[[14]][2] * 100, 1), p_mam_lci)
    testthat::expect_equal(round(p[[15]][2] * 100, 1), p_mam_uci)
    testthat::expect_equal(p[[17]][2], sum_wt)
  }
)


## When !is.null(.by) and analysis approach has different categories ----
testthat::test_that(
  "mw_estimate_prevalence_muac() works well on a dataframe with multiple survey areas with
    different categories of analysis_approach",
  {
    ### Get the prevalence estimates ----
    p <- anthro.04 |>
      mw_estimate_prevalence_muac(edema = edema, .by = province)

    ### A Province whose analysis approach is unweighted ---
    province_1 <- subset(p, province == "Province 1")

    ### A Province whose analysis approach is weighted ---
    province_2 <- subset(p, province == "Province 2")

    ### A Province whose analysis approach is add missing (NA's) ---
    province_3 <- subset(p, province == "Province 3") |>
      select(!province)

    columns_to_check <- c(
      "gam_n", "gam_p_low", "gam_p_upp", "sam_n",
      "sam_p_low", "sam_p_upp", "mam_n", "mam_p_low",
      "mam_p_upp", "wt_pop"
    )

    ### The test ----
    testthat::expect_vector(select(p, !province), size = 3, ncol(17))
    testthat::expect_s3_class(p, "tbl")
    testthat::expect_false(all(sapply(province_1[columns_to_check], \(.) all(is.na(.)))))
    testthat::expect_true(all(sapply(province_2[columns_to_check], \(.) all(is.na(.)))))
    testthat::expect_true(all(sapply(province_3[names(province_3)], \(.) all(is.na(.)))))
  }
)

## When is.null(.by) and analysis pah is add NA's ----
testthat::test_that(
  "mw_estimate_prevalence_muac() works as expected",
  {
    ### Get the prevalence estimates ----
    p <- anthro.04 |>
      subset(province == "Province 3") |>
      mw_estimate_prevalence_muac(edema = edema, .by = NULL)

    ### The test ----
    testthat::expect_s3_class(p, "tbl")
    testthat::expect_true(all(sapply(p[names(p)], \(.) all(is.na(.)))))
  }
)

## When MUAC is not in millimeters the function errors ----
testthat::test_that(
  "When MUAC is not in centimeters, the function stop execution",
  {
    testthat::expect_error(
      x <- anthro.01 |>
        mw_wrangle_age(
          age = age,
          .decimals = 2
        ) |>
        mw_wrangle_muac(
          sex = sex,
          muac = muac,
          age = age,
          .recode_sex = FALSE,
          .recode_muac = TRUE,
          .to = "cm",
          .decimals = 3
        ) |>
        mw_wrangle_wfhz(
          sex = sex,
          weight = weight,
          height = height,
          .recode_sex = F,
          .decimals = 3
        ) |>
        mw_estimate_prevalence_muac(edema = edema),
      regexp = "MUAC values must be in millimeters. Please try again."
    )
  }
)


# Test check: mw_estimate_smart_age_wt() ----
testthat::test_that(
  "mw_estimate_smart_age_wt() works well",
  {
    ## Observed results ----
    p <- anthro.04 |>
      subset(province == "Province 2") |>
      mw_estimate_smart_age_wt(raw_muac = FALSE)

    ## Expected results ----
    gam <- 11.2
    sam <- 2.0
    mam <- 9.2

    ## Tests ----
    testthat::expect_s3_class(p, "tbl")
    testthat::expect_equal(round(p[[3]][1] * 100, 1), gam)
    testthat::expect_equal(round(p[[1]][1] * 100, 1), sam)
    testthat::expect_equal(round(p[[2]][1] * 100, 1), mam)

  }
)

# Test check: mw_estimate_smart_age_wt() ----
testthat::test_that(
  "mw_estimate_smart_age_wt() works well when `raw_muac = TRUE`",
  {
  
    ## Tests ----
    testthat::expect_error(object = anthro.04 |>
      subset(province == "Province 2") |>
      mw_estimate_smart_age_wt(raw_muac = TRUE), inherit = TRUE)

  }
)


## When MUAC is not in millimeters the function errors ----
testthat::test_that(
  "When MUAC is not in centimeters, the function stop execution",
  {
    testthat::expect_error(
      x <- anthro.01 |>
        mw_wrangle_age(
          age = age,
          .decimals = 2
        ) |>
        mw_wrangle_muac(
          sex = sex,
          muac = muac,
          age = age,
          .recode_sex = FALSE,
          .recode_muac = TRUE,
          .to = "cm",
          .decimals = 3
        ) |>
        mw_wrangle_wfhz(
          sex = sex,
          weight = weight,
          height = height,
          .recode_sex = F,
          .decimals = 3
        ) |>
        mw_estimate_smart_age_wt(edema = edema),
      regexp = "MUAC values must be in millimeters. Please try again."
    )
  }
)
