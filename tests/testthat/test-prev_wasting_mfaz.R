# Test check: mw_estimate_prevalence_mfaz ----
## When std != problematic & is.null(.wt) & !is.null(edema) ----
testthat::test_that(
  "mw_estimate_prevalence_mfaz() yields correct estimates",
  {
    ### Get the prevalence estimates ----
    p <- anthro.02 |>
      mw_estimate_prevalence_mfaz(edema = edema, .by = NULL)

    ### Expected results ----
    #### GAM estimates and uncertainty ----
    n_gam <- 209
    p_gam <- 9.6
    p_gam_lci <- 8.1
    p_gam_uci <- 11.2

    #### SAM estimates and uncertainty ----
    n_sam <- 58
    p_sam <- 2.7
    p_sam_lci <- 1.9
    p_sam_uci <- 3.5

    #### MAM estimates and uncertainty ----
    n_mam <- 151
    p_mam <- 6.9
    p_mam_lci <- 5.6
    p_mam_uci <- 8.2

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

## When std != problematic & is.null(.wt) & is.null(edema) ----
testthat::test_that(
  "mw_estimate_prevalence_mfaz() yields correct estimates",
  {
    ### Get the prevalence estimates ----
    p <- anthro.02 |>
      mw_estimate_prevalence_mfaz(edema = NULL, .by = NULL)

    ### Expected results ----
    #### GAM estimates and uncertainty ----
    n_gam <- 198
    p_gam <- 9.1
    p_gam_lci <- 7.6
    p_gam_uci <- 10.6

    #### SAM estimates and uncertainty ----
    n_sam <- 45
    p_sam <- 2.1
    p_sam_lci <- 1.4
    p_sam_uci <- 2.8

    #### MAM estimates and uncertainty ----
    n_mam <- 153
    p_mam <- 7.0
    p_mam_lci <- 5.7
    p_mam_uci <- 8.3

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


## When standard deviation == problematic ----
testthat::test_that(
  "mw_estimate_prevalence_mfaz() works well on a dataframe with multiple survey areas with
    different categories on analysis_approach",
  {
    ### Get the prevalence estimates ----
    p <- anthro.04 |>
      mw_estimate_prevalence_mfaz(edema = edema, .by = province)

    ### Subset a province whose analysis approach is unweighted ---
    province_1 <- subset(p, province == "Province 1")

    ### Subset a province whose analysis approach is weighted ---
    province_3 <- subset(p, province == "Province 3")


    columns_to_check <- c(
      "gam_n", "gam_p_low", "gam_p_upp", "sam_n",
      "sam_p_low", "sam_p_upp", "mam_n", "mam_p_low",
      "mam_p_upp", "wt_pop"
    )

    ### Tests ----
    testthat::expect_vector(dplyr::select(p, !province), size = 3, ncol(17))
    testthat::expect_s3_class(p, "tbl")
    testthat::expect_false(all(sapply(province_1[columns_to_check], \(.) all(is.na(.)))))
    testthat::expect_true(all(sapply(province_3[columns_to_check], \(.) all(is.na(.)))))
  }
)
