# Test check: mw_estimate_prevalence_wfhz() ----
## When std =! problematic & !is.null(wt) & !is.null(edema) ----
testthat::test_that(
  "mw_estimate_prevalence_wfhz() yields correct estimates",
  {
    ### Get the prevalence estimates ----
    p <- mw_estimate_prevalence_wfhz(
      df = anthro.02,
      edema = edema,
      wt = wtfactor,
      .by = NULL
    )

    ### Expected results ----
    #### GAM estimates and uncertainty ----
    n_gam <- 121
    p_gam <- 4.3
    p_gam_lci <- 3.2
    p_gam_uci <- 5.4
    deff <- 1.58

    #### SAM estimates and uncertainty ----
    n_sam <- 43
    p_sam <- 0.8
    p_sam_lci <- 0.2
    p_sam_uci <- 1.3

    #### MAM estimates and uncertainty ----
    n_mam <- 78
    p_mam <- 3.5
    p_mam_lci <- 2.6
    p_mam_uci <- 4.5

    #### Sum of weigths ----
    sum_wt <- 1752680

    ### Tests ----
    testthat::expect_equal(p[[1]][1], n_gam)
    testthat::expect_equal(round(p[[2]][1] * 100, 1), p_gam)
    testthat::expect_equal(round(p[[3]][1] * 100, 1), p_gam_lci)
    testthat::expect_equal(round(p[[4]][1] * 100, 1), p_gam_uci)
    testthat::expect_equal(round(p[[5]][1], 2), deff)
    testthat::expect_equal(p[[6]][1], n_sam)
    testthat::expect_equal(round(p[[7]][1] * 100, 1), p_sam)
    testthat::expect_equal(round(p[[8]][1] * 100, 1), p_sam_lci)
    testthat::expect_equal(round(p[[9]][1] * 100, 1), p_sam_uci)
    testthat::expect_equal(p[[11]][1], n_mam)
    testthat::expect_equal(round(p[[12]][1] * 100, 1), p_mam)
    testthat::expect_equal(round(p[[13]][1] * 100, 1), p_mam_lci)
    testthat::expect_equal(round(p[[14]][1] * 100, 1), p_mam_uci)
    testthat::expect_equal(p[[16]][1], sum_wt)
  }
)

## When std != problematic & is.null(wt) & !is.null(edema) ----
testthat::test_that(
  "mw_estimate_prevalence_wfhz() yields correct estimates when survey weights is
    NULL",
  {
    ### Get the prevalence estimates ----
    p <- mw_estimate_prevalence_wfhz(
      df = wfhz.01,
      edema = edema,
      .by = NULL
    )

    ### Expected results ----
    #### GAM estimates and uncertainty ----
    n_gam <- 25
    p_gam <- 7.4
    p_gam_lci <- 3.5
    p_gam_uci <- 11.3

    #### SAM estimates and uncertainty ----
    n_sam <- 4
    p_sam <- 0.3
    p_sam_lci <- -0.3
    p_sam_uci <- 1.0

    #### MAM estimates and uncertainty ----
    n_mam <- 21
    p_mam <- 7.0
    p_mam_lci <- 3.1
    p_mam_uci <- 11.0

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

## When std =! problematic & !is.null(wt) with .by = province ----
testthat::test_that(
  "mw_estimate_prevalence_wfhz() yields correct estimates when .by is
    used",
  {
    ### Get the prevalence estimates ----
    p <- mw_estimate_prevalence_wfhz(
      df = anthro.02,
      edema = edema,
      wt = wtfactor,
      .by = province
    )

    ### Expected results for Nampula province ----
    #### GAM estimates and uncertainty ----
    n_gam <- 80
    p_gam <- 5.9
    p_gam_lci <- 4.1
    p_gam_uci <- 7.8
    deff <- 1.52

    #### SAM estimates and uncertainty ----
    n_sam <- 33
    p_sam <- 1.3
    p_sam_lci <- 0.3
    p_sam_uci <- 2.3

    #### MAM estimates and uncertainty ----
    n_mam <- 47
    p_mam <- 4.7
    p_mam_lci <- 3.1
    p_mam_uci <- 6.2

    #### Sum of weigths ----
    sum_wt <- 878704

    ### Tests ----
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

## When std == problematic & is.null(wt) ----
testthat::test_that(
  "mw_estimate_prevalence_wfhz() works well on a multi-area dataset with
  wfhz standard deviation taking different rates",
  {
    ### Get the prevalence estimates ----
    p <- anthro.03 |>
      mw_wrangle_wfhz(
        sex,
        weight,
        height,
        .recode_sex = TRUE
      ) |>
      mw_estimate_prevalence_wfhz(
        edema = edema,
        wt = NULL,
        .by = district
      )

    ### Select a district where standard deviation is rated as problematic ----
    metuge_df <- subset(p, district == "Metuge")

    ### Select a district where standard deviation is rated as problematic ----
    maravia_df <- subset(p, district == "Maravia")

    ### Select a district where standard deviation is rated as not problematic ----
    chiuta_df <- subset(p, district == "Chiuta")

    columns_to_check <- c(
      "gam_n", "gam_p_low", "gam_p_upp", "sam_n",
      "sam_p_low", "sam_p_upp", "mam_n", "mam_p_low",
      "mam_p_upp", "wt_pop"
    )

    ### Tests ----
    testthat::expect_vector(dplyr::select(p, !district), size = 4, ncol(17))
    testthat::expect_s3_class(p, "tbl")
    testthat::expect_true(all(sapply(metuge_df[columns_to_check], \(.) all(is.na(.)))))
    testthat::expect_true(all(sapply(maravia_df[columns_to_check], \(.) all(is.na(.)))))
    testthat::expect_false(all(sapply(chiuta_df[columns_to_check], \(.) all(is.na(.)))))
  }
)
