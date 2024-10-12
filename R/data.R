#'
#' A sample data of district level SMART surveys with location anonymised
#'
#' @description
#' `anthro.01` is a two-stage cluster-based survey with probability of selection
#' of clusters proportional to the size of the population. The survey employed
#' the SMART methodology.
#'
#' @format A tibble of 1,191 rows and 11 columns.
#'
#' | **Variable** | **Description** |
#' | :--- | :--- |
#' | *area* | Location where the survey took place |
#' | *dos* | Survey date |
#' | *cluster* | Primary sampling unit |
#' | *team* | Enumerator IDs |
#' | *sex* | Sex, "m" = boys, "f" = girls |
#' | *dob* | Date of birth |
#' | *age* | Age in months, typically estimated using local event calendars |
#' | *weight* | Weight (kg) |
#' | *height* | Height (cm) |
#' | *edema* | Edema, "n" = no, "y" = yes |
#' | *muac* | Mid-upper arm circumference (mm) |
#'
#' @source Anonymous
#'
#' @examples
#' anthro.01
#'
#'
"anthro.01"

#'
#' A sample of an already wrangled survey data
#'
#' @description
#' `anthro.02` is about a household budget survey conducted in Mozambique in
#' 2019/2020, known as IOF (*Inquérito ao Orçamento Familiar* in Portuguese).*IOF*
#' is a two-stage cluster-based survey, representative at province level (admin 2),
#'  with probability of the selection of the clusters proportional to the size of
#'  the population. Its data collection spans for a period of 12 months.
#'
#' @format A tibble of 2,267 rows and 14 columns.
#'
#' |**Variable** | **Description** |
#' | :--- | :---|
#' | *province* | The administrative unit (admin 1) where data was collected. |
#' | *strata* | Rural and Urban |
#' | *cluster* | Primary sampling unit |
#' | *sex* | Sex, "m" = boys, "f" = girls |
#' | *age* | calculated age in months with two decimal places |
#' | *weight* | Weight (kg) |
#' | *height* | Height (cm) |
#' | *edema* | Edema, "n" = no, "y" = yes |
#' | *muac* | Mid-upper arm circumference (mm) |
#' | *wtfactor* | Survey weights |
#' | *wfhz* | Weight-for-height z-scores with 3 decimal places |
#' | *flag_wfhz* | Flagged observations. 1=flagged, 0=not flagged |
#' | *mfaz* | MUAC-for-age z-scores with 3 decimal places |
#' | *flag_mfaz* | Flagged observations. 1=flagged, 0=not flagged |
#'
#' @source Mozambique National Institute of Statistics. The data is publicly
#' available at <https://mozdata.ine.gov.mz/index.php/catalog/88#metadata-data_access>.
#' Data was wrangled using this package's wranglers. Details about survey design
#' can be gotten from: <https://mozdata.ine.gov.mz/index.php/catalog/88#metadata-sampling>
#'
#' @examples
#' anthro.02
#'
"anthro.02"


#'
#' A sample data of district level SMART surveys conducted in Mozambique
#'
#' @description
#' `anthro.03` contains survey data of four districts. Each district's dataset
#' presents distinct data quality scenarios that requires tailored prevalence
#' analysis approach: two districts show a problematic WFHZ standard deviation
#' whilst the remaining are all within range.
#'
#' This sample data demonstrates the use of prevalence functions on multi-area
#' survey data, where there is variations in the standard deviation rating.
#' As a result, different analyses approaches are required for each area
#' to ensure accurate estimation.
#'
#' @format A tibble of 943 x 9.
#'
#' |**Variable** | **Description** |
#' | :--- | :---|
#' | *district* | The administrative unit (admin 1) where data was collected. |
#' | *cluster* | Primary sampling unit |
#' | *team* | Survey teams |
#' | *sex* | Sex, "m" = boys, "f" = girls |
#' | *age* | calculated age in months with two decimal places |
#' | *weight* | Weight (kg) |
#' | *height* | Height (cm) |
#' | *edema* | Edema, "n" = no, "y" = yes |
#' | *muac* | Mid-upper arm circumference (mm) |
#'
#' @source Anonymous
#'
#' @examples
#' anthro.03
#'
#'
"anthro.03"


#'
#'
#' A sample data of a community-based sentinel site from an anonymized location
#'
#' @description
#' `anthro.04` was generated from a community-based sentinel site conducted
#' across three provinces. Each province's dataset presents distinct
#' data quality scenarios, requiring tailored prevalence analysis:
#' "Province 3" has problematic MFAZ standard deviation and age ratio tests;
#' "Province 2" shows a problematic age ratio but acceptable MFAZ standard
#' deviation; lastly, "Province 1" has both tests within acceptable ranges.
#'
#' This sample data demonstrates the use of prevalence functions on multi-area
#' survey data, where variations in the standard deviation ratings exist.
#' As a result, different analytical approaches are required for each area
#' to ensure accurate interpretation.
#'
#' @format A tibble of 3,002 x 8.
#'
#' |**Variable** | **Description** |
#' | :--- | :---|
#' | *province* | location where data was collected |
#' | *cluster* | Primary sampling unit |
#' | *sex* | Sex, "m" = boys, "f" = girls |
#' | *age* | calculated age in months with two decimal places |
#' | *muac* | Mid-upper arm circumference (mm) |
#' | *edema* | Edema, "n" = no, "y" = yes |
#' | *mfaz* | MUAC-for-age z-scores with 3 decimal places |
#' | *flag_mfaz* | Flagged observations. 1=flagged, 0=not flagged |
#'
#' @source Anonymous
#'
#' @examples
#' anthro.04
#'
#'
"anthro.04"


#'
#' A sample SMART survey data with WFHZ standard deviation rated as problematic
#'
#' @format A tibble with 303 rows and 6 columns.
#'
#' | **Variable** | **Description** |
#' | :--- | :---|
#' | *cluster* | Primary sampling unit |
#' | *sex* | Sex, "m" = boys, "f" = girls |
#' | *age* | calculated age in months with two decimal places |
#' | *edema* | Edema, "n" = no, "y" = yes |
#' | *wfhz* | MUAC-for-age z-scores with 3 decimal places |
#' | *flag_wfhz* | Flagged observations. 1=flagged, 0=not flagged |
#'
#' @source Anonymous
#'
#' @examples
#' wfhz.01
#'
#'
"wfhz.01"


#'
#' A sample MUAC screening data from an anonymized setting
#'
#' @format A tibble with 661 rows and 4 columns.
#'
#' |**Variable** | **Description** |
#' | :--- | :---|
#' | *sex* | Sex, "m" = boys, "f" = girls |
#' | *months* | calculated age in months with two decimal places |
#' | *edema* | Edema, "n" = no, "y" = yes |
#' | *muac* | Mid-upper arm circumference (mm) |
#'
#' @source Anonymous
#'
#' @examples
#' mfaz.01
#'
"mfaz.01"

#'
#' A sample SMART survey data with MUAC
#'
#' @format A tibble with 303 rows and 7 columns.
#'
#' |**Variable** | **Description** |
#' | :--- | :---|
#' | *cluster* | Primary sampling unit |
#' | *sex* | Sex, "m" = boys, "f" = girls |
#' | *age* | calculated age in months with two decimal places |
#' | *edema* | Edema, "n" = no, "y" = yes |
#' | *mfaz* | MUAC-for-age z-scores with 3 decimal places |
#' | *flag_mfaz* | Flagged observations. 1=flagged, 0=not flagged |
#'
#' @source Anonymous
#'
#' @examples
#' mfaz.02
#'
"mfaz.02"
