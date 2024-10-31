# Test check: apply_probit_approach() ----
local({
  x <- anthro.03 |>
    mw_wrangle_wfhz(sex, weight, height, .recode_sex = TRUE) |>
    subset(district == "Metuge")

    p_gam <- apply_probit_approach(x$wfhz, .status = "gam")
    p_sam <- apply_probit_approach(x$wfhz, .status = "sam")

    ## The test ----
    testthat::test_that(
      "apply_probit_approach() works",
      {
        testthat::expect_vector(c(p_gam, p_sam), ptype = double(), size = 2)
      }
    )
})

# Test check: compute_probit_prevalence() ----
local({
  p <- anthro.03 |>
    mw_wrangle_wfhz(sex, weight, height, .recode_sex = TRUE) |>
    subset(district == "Metuge") |>
    compute_probit_prevalence()

  ## The test ----
  testthat::test_that(
    "compute_probit_prevalence() return the correct ouput object",
    {
      testthat::expect_s3_class(p, class = "tbl", exact = FALSE)
      testthat::expect_length(p, 3)
      testthat::expect_vector(p, nrow(1))
    }
  )
})

# Test check: compute_probit_prevalence(.summary_by = district) ----
local({
  p <- anthro.03 |>
    mw_wrangle_wfhz(sex, weight, height, .recode_sex = TRUE) |>
    subset(district == "Metuge" | district == "Maravia") |>
    compute_probit_prevalence(.summary_by = district)

  ## The test ----
  testthat::test_that(
    "compute_probit_prevalence() return the correct ouput object",
    {
      testthat::expect_length(p, 4)
      testthat::expect_vector(p, nrow(2))
    }
  )
})

# Test check: compute_wfhz_prevalence() ----
## When std =! problematic & !is.null(.wt) & !is.null(.edema) ----
local({

  ### Get the prevalence estimates ----
  p <- anthro.02 |>
    compute_wfhz_prevalence(.edema = edema, .wt = "wtfactor", .summary_by = NULL)

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

  ### The test ----
  testthat::test_that(
    "compute_wfhz_prevalence() yields correct estimates",
    {
      testthat::expect_equal(p[[1]][1], n_gam)
      testthat::expect_equal(round(p[[2]][1]*100, 1), p_gam)
      testthat::expect_equal(round(p[[3]][1]*100, 1), p_gam_lci)
      testthat::expect_equal(round(p[[4]][1]*100, 1), p_gam_uci)
      testthat::expect_equal(round(p[[5]][1], 2), deff)
      testthat::expect_equal(p[[6]][1], n_sam)
      testthat::expect_equal(round(p[[7]][1]*100, 1), p_sam)
      testthat::expect_equal(round(p[[8]][1]*100, 1), p_sam_lci)
      testthat::expect_equal(round(p[[9]][1]*100, 1), p_sam_uci)
      testthat::expect_equal(p[[11]][1], n_mam)
      testthat::expect_equal(round(p[[12]][1]*100, 1), p_mam)
      testthat::expect_equal(round(p[[13]][1]*100, 1), p_mam_lci)
      testthat::expect_equal(round(p[[14]][1]*100, 1), p_mam_uci)
      testthat::expect_equal(p[[16]][1], sum_wt)
    }
  )
})

## When std =! problematic & !is.null(.wt) & is.null(.edema) ----
local({

  ### Get the prevalence estimates ----
  p <- anthro.02 |>
    compute_wfhz_prevalence(.edema = NULL, .wt = "wtfactor", .summary_by = NULL)

  ### Expected results ----
  #### GAM estimates and uncertainty ----
  n_gam <- 107
  p_gam <- 3.5
  p_gam_lci <- 2.6
  p_gam_uci <- 4.5
  deff <- 1.43

  #### SAM estimates and uncertainty ----
  n_sam <- 29
  p_sam <- 0.0
  p_sam_lci <- 0.0
  p_sam_uci <- 0.0

  #### MAM estimates and uncertainty ----
  n_mam <- 78
  p_mam <- 3.5
  p_mam_lci <- 2.6
  p_mam_uci <- 4.5

  #### Sum of weigths ----
  sum_wt <- 1752680

  ### The test ----
  testthat::test_that(
    "compute_wfhz_prevalence() yields correct estimates when edema is NULL",
    {
      testthat::expect_equal(p[[1]][1], n_gam)
      testthat::expect_equal(round(p[[2]][1]*100, 1), p_gam)
      testthat::expect_equal(round(p[[3]][1]*100, 1), p_gam_lci)
      testthat::expect_equal(round(p[[4]][1]*100, 1), p_gam_uci)
      testthat::expect_equal(round(p[[5]][1], 2), deff)
      testthat::expect_equal(p[[6]][1], n_sam)
      testthat::expect_equal(round(p[[7]][1]*100, 1), p_sam)
      testthat::expect_equal(round(p[[8]][1]*100, 1), p_sam_lci)
      testthat::expect_equal(round(p[[9]][1]*100, 1), p_sam_uci)
      testthat::expect_equal(p[[11]][1], n_mam)
      testthat::expect_equal(round(p[[12]][1]*100, 1), p_mam)
      testthat::expect_equal(round(p[[13]][1]*100, 1), p_mam_lci)
      testthat::expect_equal(round(p[[14]][1]*100, 1), p_mam_uci)
      testthat::expect_equal(p[[16]][1], sum_wt)
    }
  )
})


## When std != problematic & is.null(.wt) & !is.null(.edema) ----
local({

  ### Get the prevalence estimates ----
  p <- wfhz.01 |>
    compute_wfhz_prevalence(.edema = edema, .summary_by = NULL)

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

  ### The test ----
  testthat::test_that(
    "compute_wfhz_prevalence() yields correct estimates when survey weights is
    NULL",
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

## When std =! problematic & !is.null(.wt) with .summary_by = province ----
local({

  ### Get the prevalence estimates ----
  p <- anthro.02 |>
    compute_wfhz_prevalence(
      .edema = edema,
      .wt = "wtfactor",
      .summary_by = province
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

  ### The test ----
  testthat::test_that(
    "compute_wfhz_prevalence() yields correct estimates when .summary_by is
    used",
    {
      testthat::expect_equal(p[[2]][2], n_gam)
      testthat::expect_equal(round(p[[3]][2]*100, 1), p_gam)
      testthat::expect_equal(round(p[[4]][2]*100, 1), p_gam_lci)
      testthat::expect_equal(round(p[[5]][2]*100, 1), p_gam_uci)
      testthat::expect_equal(round(p[[6]][2], 2), deff)
      testthat::expect_equal(p[[7]][2], n_sam)
      testthat::expect_equal(round(p[[8]][2]*100, 1), p_sam)
      testthat::expect_equal(round(p[[9]][2]*100, 1), p_sam_lci)
      testthat::expect_equal(round(p[[10]][2]*100, 1), p_sam_uci)
      testthat::expect_equal(p[[12]][2], n_mam)
      testthat::expect_equal(round(p[[13]][2]*100, 1), p_mam)
      testthat::expect_equal(round(p[[14]][2]*100, 1), p_mam_lci)
      testthat::expect_equal(round(p[[15]][2]*100, 1), p_mam_uci)
      testthat::expect_equal(p[[17]][2], sum_wt)
    }
  )
})

## When std == problematic & is.null(.wt) ----
# To access problematic SD's in antho.03, .summary_by has to be not null ----
local({

  ### Get the prevalence estimates ----
  p <- anthro.03 |>
    mw_wrangle_wfhz(sex, weight, height, .recode_sex = TRUE) |>
    compute_wfhz_prevalence(.edema = edema, .summary_by = district)

  ### Subset the dataframe for the district "Metuge" with problematic SD ----
  metuge_df <- subset(p, district == "Metuge")

  ### Subset the dataframe for the district "Maravia" with problematic SD ----
  maravia_df <- subset(p, district == "Maravia")

  ### Subset the dataframe for the district "Chiuta" with != problematic SD ---
  chiuta_df <- subset(p, district == "Chiuta")

  columns_to_check <- c("gam_n", "gam_p_low", "gam_p_upp", "sam_n",
                       "sam_p_low", "sam_p_upp", "mam_n", "mam_p_low",
                       "mam_p_upp", "wt_pop")

  ### The test ----

  testthat::test_that(
    "compute_wfhz_prevalence() works well on a dataframe with multiple survey areas with
    different SD's classification",
    {
      testthat::expect_vector(dplyr::select(p, !district), size = 4, ncol(17))
      testthat::expect_s3_class(p, "tbl")
      testthat::expect_true(all(sapply(metuge_df[columns_to_check], \(.) all(is.na(.)))))
      testthat::expect_true(all(sapply(maravia_df[columns_to_check], \(.) all(is.na(.)))))
      testthat::expect_false(all(sapply(chiuta_df[columns_to_check], \(.) all(is.na(.)))))
    }
  )
})
