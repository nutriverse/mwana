# Compare z-scores produced by ENA and produced by zscorer ---------------------

## Load libraries ----
library(zscorer)
library(anthro)      ## WHO package for z-score calculations
library(lubridate)


## Read dataset ----
ena_df <- read.csv("data-raw/ENA_generated_zscores.csv")


## Process dataset ----

### Calculate z-scores using zscorer and anthro package and compare to ENA ----

df <- ena_df |>
  dplyr::mutate(
    sex = ifelse(SEX == "m", 1, 2),
    age_in_days_1 = MONTHS * (365.25 / 12),
    age_in_days_2 = floor(MONTHS) * (365.25 / 12),
    age_in_days_3 = as.Date(SURVDATE, format = "%m/%d/%Y") - 
      as.Date(BIRTHDAT, format = "%m/%d/%Y"),
    age_in_days_4 = lubridate::day(
      as.period(
        interval(
          start = as.Date(BIRTHDAT, format = "%m/%d/%Y"), 
          end = as.Date(SURVDATE, format = "%m/%d/%Y")
        )
      )
    )
  ) |>
  dplyr::select(
    sex, dplyr::starts_with("age"), 
    wt = WEIGHT, ht = HEIGHT, oed = EDEMA, muac = MUAC, wfhz_ena = WHZ.WHO 
  )

df <- addWGSR(
  df, sex = "sex", firstPart = "wt", secondPart = "ht", 
  index = "wfh", output = "wfhz_zscorer", digits = 3
) |>
  dplyr::mutate(
    wfhz_ena_2 = round(wfhz_ena, digits = 2),
    wfhz_zscorer_2 = round(wfhz_zscorer, digits = 2),
    wfhz_anthro = anthro_zscores(sex = sex, weight = wt, lenhei = ht)$zwfl,
    diff1 = wfhz_ena - wfhz_zscorer,
    diff2 = wfhz_ena_2 - wfhz_zscorer_2,
    diff3 = wfhz_ena_2 - wfhz_anthro,
    diff4 = wfhz_anthro - wfhz_zscorer_2,
    sam_ena = ifelse(wfhz_ena < -3, 1, 0),
    mam_ena= ifelse(wfhz_ena >= -3 & wfhz_ena < -2, 1, 0),
    sam_zscorer = ifelse(wfhz_zscorer < -3, 1, 0),
    mam_zscorer = ifelse(wfhz_zscorer >= -3 & wfhz_ena < -2, 1, 0),
    sam_anthro = ifelse(wfhz_anthro < -3, 1, 0),
    mam_anthro = ifelse(wfhz_anthro >= -3 & wfhz_ena < -2, 1, 0),
  )

### Tabulate sam and gam per method ----

sam_compare <- dplyr::count(df, sam_ena, name = "n_ena") |>
  dplyr::rename(sam = sam_ena) |>
  dplyr::left_join(
    dplyr::count(df, sam_zscorer, name = "n_zscorer") |>
      dplyr::rename(sam = sam_zscorer)
  ) |>
  dplyr::left_join(
    dplyr::count(df, sam_anthro, name = "n_anthro") |>
      dplyr::rename(sam = sam_anthro)
  )

mam_compare <- dplyr::count(df, mam_ena, name = "n_ena") |>
  dplyr::rename(mam = mam_ena) |>
  dplyr::left_join(
    dplyr::count(df, mam_zscorer, name = "n_zscorer") |>
      dplyr::rename(mam = mam_zscorer)
  ) |>
  dplyr::left_join(
    dplyr::count(df, mam_anthro, name = "n_anthro") |>
      dplyr::rename(mam = mam_anthro)
  )
  
  
