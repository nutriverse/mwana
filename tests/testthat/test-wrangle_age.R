### Test check: calculate_age_in_months() ----

testthat::test_that(
  "calculate_age_in_months() does the job as expected",

  {
    #### Sample data ----
    ##### Survey date
    surv_date <- as.Date(c(
      "2024-01-05", "2024-01-05", "2024-01-05", "2024-01-08", "2024-01-08",
      "2024-01-08", "2024-01-10", "2024-01-10", "2024-01-10", "2024-01-11",
      "2024-01-11", "2024-01-11", "2024-01-12", "2024-01-12", "2024-01-12"
    ))
    ##### Birthdate ----
    bir_date <- as.Date(c(
      "2022-04-04", "2021-05-01", "2023-05-24", "2017-12-12", NA,
      "2020-12-12", "2022-04-04", "2021-05-01", "2023-05-24", "2020-12-12",
      "2021-05-01", "2020-12-12", "2022-04-04", "2021-05-01", "2023-05-24"
    ))
    ##### Age in months ----
    age <- NA_integer_

    ##### Create a data frame ----
    df <- data.frame(surv_date, bir_date, age)

    #### Expected results ----
    x <- c(21.06, 32.16, 7.43, NA, NA, 36.86, 21.22,
           32.33, 7.59, 36.96, 32.36, 36.96, 21.29, 32.39, 7.66)

    #### Observed results ----
    df <- df |>
      mutate(
        age_mo = calculate_age_in_months(dos = surv_date, dob = bir_date)
      )

    #### The test ----
    testthat::expect_vector(df[["age_mo"]], size = 15)
    testthat::expect_equal(df[["age_mo"]], x)
  }
)


### Test check: mw_wrangle_age() ----

testthat::test_that(
  "mw_wrangle_age() does works super",
  {

    #### Sample data ----
    df <- data.frame(
      surv_date = as.Date(c(
        "2023-01-01", "2023-01-01", "2023-01-01", "2023-01-01", "2023-01-01")),
      birth_date = as.Date(c(
        "2019-01-01", NA, "2018-03-20", "2019-11-05", "2021-04-25")),
      age = c(NA, 36, NA, NA, NA)
    )

    #### Expected results ----
    w <- c(1461.12, 1095.84, 1748.17, 1153.07, 616.11)

    #### Observed results ----
    df <- mw_wrangle_age(df, dos = surv_date, dob = birth_date, age = age)

    #### The test ----
    testthat::expect_type(df, "list")
    testthat::expect_vector(df[["age_days"]], size = 5)
    testthat::expect_equal(df[["age_days"]], w)
  }
)
