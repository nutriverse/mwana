# Test check: compute_mfaz_prevalence ----

## When std != problematic & is.null(.wt) & !is.null(.edema) ----
local({

  ### Get the prevalence estimates ----
  p <- anthro.04 |>
    compute_mfaz_prevalence(.edema = edema, .summary_by = NULL)

  ### Expected results ----
  #### GAM estimates and uncertainty ----
  n_gam <- 330
  p_gam <- 10.7
  p_gam_lci <- 8.7
  p_gam_uci <- 12.7

  #### SAM estimates and uncertainty ----
  n_sam <- 53
  p_sam <- 1.4
  p_sam_lci <- 0.9
  p_sam_uci <- 2.0

  #### MAM estimates and uncertainty ----
  n_mam <- 277
  p_mam <- 9.3
  p_mam_lci <- 7.5
  p_mam_uci <- 11.1

  ### The test ----
  testthat::test_that(
    "compute_mfaz_prevalence() yields correct estimates",
    {
      testthat::expect_equal(p[[1]][1], n_gam)
      testthat::expect_equal(round(p[[2]][1]*100, 1), p_gam)
      testthat::expect_equal(round(p[[3]][1]*100, 1), p_gam_lci)
      testthat::expect_equal(round(p[[4]][1]*100, 1), p_gam_uci)
      testthat::expect_equal(p[[6]][1], n_sam)
      testthat::expect_equal(round(p[[7]][1]*100, 1), p_sam)
      testthat::expect_equal(round(p[[8]][1]*100, 1), p_sam_lci)
      testthat::expect_equal(round(p[[9]][1]*100, 1), p_sam_uci)
      testthat::expect_equal(p[[11]][1], n_mam)
      testthat::expect_equal(round(p[[12]][1]*100, 1), p_mam)
      testthat::expect_equal(round(p[[13]][1]*100, 1), p_mam_lci)
      testthat::expect_equal(round(p[[14]][1]*100, 1), p_mam_uci)
    }
  )
})


## When standard deviation == problematic ----
local({

  ### Get the prevalence estimates ----
  p <- anthro.04 |>
    compute_mfaz_prevalence(.edema = edema, .summary_by = province)

  ### Subset a province whose analysis approach is unweighted ---
  province_1 <- subset(p, province == "Province 1")

  ### Subset a province whose analysis approach is weighted ---
  province_3 <- subset(p, province == "Province 3")


  columns_to_check <- c("gam_n", "gam_p_low", "gam_p_upp", "sam_n",
                        "sam_p_low", "sam_p_upp", "mam_n", "mam_p_low",
                        "mam_p_upp", "wt_pop")

  ### The test ----

  testthat::test_that(
    "compute_mfaz_prevalence() works well on a dataframe with multiple survey areas with
    different categories on analysis_approach",
    {
      testthat::expect_vector(dplyr::select(p, !province), size = 3, ncol(17))
      testthat::expect_s3_class(p, "tbl")
      testthat::expect_false(all(sapply(province_1[columns_to_check], \(.) all(is.na(.)))))
      testthat::expect_true(all(sapply(province_3[columns_to_check], \(.) all(is.na(.)))))
    }
  )
})


