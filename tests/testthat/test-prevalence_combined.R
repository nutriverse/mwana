## Test check: compute_combined_prevalence() ----
### When std != problematic & muac_analysis == unweighted & !is.null(.wt) ----
local({

  #### Get prevalence estimates ----
  p <- anthro.02 |>
    compute_combined_prevalence(.edema = edema, .wt = "wtfactor")

  #### Expected results ----
  ##### combined GAM estimates and uncertainty ----
  n_cgam <- 199
  p_cgam <- 7.1
  p_cgam_lci <- 5.6
  p_cgam_uci <- 8.5
  deff <- 1.72

  ##### combined SAM estimates and uncertainty ----
  n_csam <- 68
  p_csam <- 1.5
  p_csam_lci <- 0.8
  p_csam_uci <- 2.3

  ##### combined MAM estimates and uncertainty ----
  n_cmam <- 145
  p_cmam <- 6.0
  p_cmam_lci <- 4.7
  p_cmam_uci <- 7.3

  ##### Sum of weights -----
  sum_wt <- 1738110

  #### The test ----
  testthat::test_that(
    "compute_combined_prevalence() yields correct estimates when edema and
    survey weights are supplied",
    {
      testthat::expect_equal(p[[1]][1], n_cgam)
      testthat::expect_equal(round(p[[2]][1]*100, 1), p_cgam)
      testthat::expect_equal(round(p[[3]][1]*100, 1), p_cgam_lci)
      testthat::expect_equal(round(p[[4]][1]*100, 1), p_cgam_uci)
      testthat::expect_equal(round(p[[5]][1], 2), deff)
      testthat::expect_equal(p[[6]][1], n_csam)
      testthat::expect_equal(round(p[[7]][1]*100, 1), p_csam)
      testthat::expect_equal(round(p[[8]][1]*100, 1), p_csam_lci)
      testthat::expect_equal(round(p[[9]][1]*100, 1), p_csam_uci)
      testthat::expect_equal(p[[11]][1], n_cmam)
      testthat::expect_equal(round(p[[12]][1]*100, 1), p_cmam)
      testthat::expect_equal(round(p[[13]][1]*100, 1), p_cmam_lci)
      testthat::expect_equal(round(p[[14]][1]*100, 1), p_cmam_uci)
      testthat::expect_equal(round(p[[16]][1]), sum_wt)
    }
  )
})

### When std != problematic & muac_analysis == unweighted & !is.null(.wt) ----
local({

  #### Get prevalence estimates ----
  p <- anthro.02 |>
    compute_combined_prevalence(.edema = NULL, .wt = "wtfactor")

  #### Expected results ----
  ##### combined GAM estimates and uncertainty ----
  n_cgam <- 187
  p_cgam <- 6.4
  p_cgam_lci <- 5.0
  p_cgam_uci <- 7.8
  deff <- 1.67

  ##### combined SAM estimates and uncertainty ----
  n_csam <- 55
  p_csam <- 0.8
  p_csam_lci <- 0.3
  p_csam_uci <- 1.2

  ##### combined MAM estimates and uncertainty ----
  n_cmam <- 146
  p_cmam <- 6.1
  p_cmam_lci <- 4.8
  p_cmam_uci <- 7.4

  #### Sum of weights ----
  sum_wt <- 1738110

  #### The test ----
  testthat::test_that(
    "compute_combined_prevalence() yields correct estimates when edema is NULL",
    {
      testthat::expect_equal(p[[1]][1], n_cgam)
      testthat::expect_equal(round(p[[2]][1]*100, 1), p_cgam)
      testthat::expect_equal(round(p[[3]][1]*100, 1), p_cgam_lci)
      testthat::expect_equal(round(p[[4]][1]*100, 1), p_cgam_uci)
      testthat::expect_equal(p[[6]][1], n_csam)
      testthat::expect_equal(round(p[[7]][1]*100, 1), p_csam)
      testthat::expect_equal(round(p[[8]][1]*100, 1), p_csam_lci)
      testthat::expect_equal(round(p[[9]][1]*100, 1), p_csam_uci)
      testthat::expect_equal(p[[11]][1], n_cmam)
      testthat::expect_equal(round(p[[12]][1]*100, 1), p_cmam)
      testthat::expect_equal(round(p[[13]][1]*100, 1), p_cmam_lci)
      testthat::expect_equal(round(p[[14]][1]*100, 1), p_cmam_uci)
      testthat::expect_equal(round(p[[16]][1]), sum_wt)
    }
  )
})


### When is.null(.wt) ----
local({

  #### Get prevalence estimates ----
  p <- anthro.02 |>
    compute_combined_prevalence(.edema = edema)

  #### Expected results ----
  ##### combined GAM estimates and uncertainty ----
  n_cgam <- 199
  p_cgam <- 6.8
  p_cgam_lci <- 5.7
  p_cgam_uci <- 8.0

  ##### combined SAM estimates and uncertainty ----
  n_csam <- 68
  p_csam <- 1.3
  p_csam_lci <- 0.8
  p_csam_uci <- 1.8

  ##### combined MAM estimates and uncertainty ----
  n_cmam <- 145
  p_cmam <- 5.9
  p_cmam_lci <- 4.8
  p_cmam_uci <- 7.0

  #### The test ----
  testthat::test_that(
    "compute_combined_prevalence() yields correct estimates",
    {
      testthat::expect_equal(p[[1]][1], n_cgam)
      testthat::expect_equal(round(p[[2]][1]*100, 1), p_cgam)
      testthat::expect_equal(round(p[[3]][1]*100, 1), p_cgam_lci)
      testthat::expect_equal(round(p[[4]][1]*100, 1), p_cgam_uci)
      testthat::expect_equal(p[[6]][1], n_csam)
      testthat::expect_equal(round(p[[7]][1]*100, 1), p_csam)
      testthat::expect_equal(round(p[[8]][1]*100, 1), p_csam_lci)
      testthat::expect_equal(round(p[[9]][1]*100, 1), p_csam_uci)
      testthat::expect_equal(p[[11]][1], n_cmam)
      testthat::expect_equal(round(p[[12]][1]*100, 1), p_cmam)
      testthat::expect_equal(round(p[[13]][1]*100, 1), p_cmam_lci)
      testthat::expect_equal(round(p[[14]][1]*100, 1), p_cmam_uci)
    }
  )
})

### When !is.null(.wt) with .summary_by = province ----
local({

  #### Get prevalence estimates ----
  p <- anthro.02 |>
    compute_combined_prevalence(
      .edema = edema,
      .wt = "wtfactor",
      .summary_by = province
    )

  #### Expected results for Nampula province ----
  ##### GAM estimates and uncertainty ----
  n_cgam <- 121
  p_cgam <- 8.4
  p_cgam_lci <- 6.0
  p_cgam_uci <- 10.9
  deff <- 1.87

  ##### SAM estimates and uncertainty ----
  n_csam <- 47
  p_csam <- 2.0
  p_csam_lci <- 0.7
  p_csam_uci <- 3.3

  ##### MAM estimates and uncertainty ----
  n_cmam <- 80
  p_cmam <- 6.8
  p_cmam_lci <- 4.7
  p_cmam_uci <- 9.0

  ##### Sum of survey weights -----
  sum_wt <- 869504
  #### The test ----
  testthat::test_that(
    "compute_combined_prevalence() yields correct estimates when .summary_by is
    used",
    {
      testthat::expect_equal(p[[2]][2], n_cgam)
      testthat::expect_equal(round(p[[3]][2]*100, 1), p_cgam)
      testthat::expect_equal(round(p[[4]][2]*100, 1), p_cgam_lci)
      testthat::expect_equal(round(p[[5]][2]*100, 1), p_cgam_uci)
      testthat::expect_equal(round(p[[6]][2], 2), deff)
      testthat::expect_equal(p[[7]][2], n_csam)
      testthat::expect_equal(round(p[[8]][2]*100, 1), p_csam)
      testthat::expect_equal(round(p[[9]][2]*100, 1), p_csam_lci)
      testthat::expect_equal(round(p[[10]][2]*100, 1), p_csam_uci)
      testthat::expect_equal(p[[12]][2], n_cmam)
      testthat::expect_equal(round(p[[13]][2]*100, 1), p_cmam)
      testthat::expect_equal(round(p[[14]][2]*100, 1), p_cmam_lci)
      testthat::expect_equal(round(p[[15]][2]*100, 1), p_cmam_uci)
      testthat::expect_equal(round(p[[17]][2]), sum_wt)
    }
  )
})

### When !is.null(.summary_by) and analysis approach has different categories ----
local({

  ### Get the prevalence estimates ----
  p <- anthro.03 |>
    mw_wrangle_age(
      age = age,
      .decimals = 2
      ) |>
    process_muac_data(
      sex = sex,
      muac = muac,
      age = "age",
      .recode_sex = TRUE,
      .recode_muac = TRUE,
      unit = "cm"
    ) |>
    dplyr::mutate(muac = recode_muac(muac, unit = "mm")) |>
    process_wfhz_data(
      sex = sex,
      weight = weight,
      height = height,
      .recode_sex = TRUE) |>
    compute_combined_prevalence(
      .edema = edema,
      .summary_by = district
      )

  ### Subset a district where a normal analysis should be computed ----
  CB <- subset(p, district == "Cahora-Bassa")

  ## Subset a district where NA should be thrown ----
  M <- subset(p, district == "Maravia")|> dplyr::select(!district)

  ### The test ----
  testthat::test_that(
    "compute_combined_prevalence() works well on a dataframe with multiple survey areas with
    different analysis approach required",
    {
      testthat::expect_vector(dplyr::select(p, !district), size = 4, ncol(17))
      testthat::expect_s3_class(p, "tbl")
      testthat::expect_false(all(sapply(CB[names(CB)], \(.) all(is.na(.)))))
      testthat::expect_true(all(sapply(M[names(M)], \(.) all(is.na(.)))))
    }
  )
})


