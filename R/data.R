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
#' | *area* | Survey location |
#' | *dos* | Survey date |
#' | *cluster* | Primary sampling unit |
#' | *team* | Enumerator IDs |
#' | *sex* | Sex; "m" = boys, "f" = girls |
#' | *dob* | Date of birth |
#' | *age* | Age in months, typically estimated using local event calendars |
#' | *weight* | Weight in kilograms |
#' | *height* | Height in centimetres |
#' | *edema* | Edema; "n" = no edema, "y" = with edema |
#' | *muac* | Mid-upper arm circumference in millimetres |
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
#' A household budget survey data conducted in Mozambique in 2019/2020, known as
#' *IOF* (*Inquérito ao Orçamento Familiar* in Portuguese). *IOF* is a two-stage 
#' cluster-based survey, representative at province level (second administrative 
#' level), with probability of the selection of the clusters proportional to the 
#' size of the population. Its data collection spans for a period of 12 months.
#'
#' @format A tibble of 2,267 rows and 14 columns.
#'
#' |**Variable** | **Description** |
#' | :--- | :---|
#' | *province* | The administrative unit level 1 where data was collected |
#' | *strata* | Rural or Urban |
#' | *cluster* | Primary sampling unit |
#' | *sex* | Sex; "m" = boys, "f" = girls |
#' | *age* | Calculated age in months with two decimal places |
#' | *weight* | Weight in kilograms |
#' | *height* | Height in centimetres |
#' | *edema* | Edema; "n" = no edema, "y" = with edema |
#' | *muac* | Mid-upper arm circumference in millimetres |
#' | *wtfactor* | Survey weights |
#' | *wfhz* | Weight-for-height z-scores with 3 decimal places |
#' | *flag_wfhz* | Flagged WFHZ value. 1 = flagged, 0 = not flagged |
#' | *mfaz* | MUAC-for-age z-scores with 3 decimal places |
#' | *flag_mfaz* | Flagged MFAZ value. 1 = flagged, 0 = not flagged |
#'
#' @source Mozambique National Institute of Statistics. The data is publicly
#' available at <https://mozdata.ine.gov.mz/index.php/catalog/88#metadata-data_access>.
#' Data was wrangled using this package's wranglers. Details about survey design
#' can be read from: <https://mozdata.ine.gov.mz/index.php/catalog/88#metadata-sampling>
#'
#' @examples
#' anthro.02
#'
"anthro.02"


#'
#' A sample data of district level SMART surveys conducted in Mozambique
#'
#' @description
#' `anthro.03` contains survey data of four districts. Each district data set
#' presents distinct data quality scenarios that require a specific prevalence
#' analysis approach. Data from two districts have a problematic WFHZ standard 
#' deviation. The data from the remaining two districts are all within range.
#'
#' This sample data is useful to demonstrate the use of the prevalence functions
#' on a multiple-domain survey data where there can be variations in the rating
#' of acceptability of the standard deviation, hence requiring different 
#' analytical approach for each survey domain to ensure accurate estimation.
#'
#' @format A tibble of 943 x 9.
#'
#' |**Variable** | **Description** |
#' | :--- | :---|
#' | *district* | Survey location |
#' | *cluster* | Primary sampling unit |
#' | *team* | Survey teams |
#' | *sex* | Sex; "m" = boys, "f" = girls |
#' | *age* | Calculated age in months with two decimal places |
#' | *weight* | Weight in kilograms |
#' | *height* | Height in centimetres |
#' | *edema* | Edema; "n" = no edema, "y" = with edema |
#' | *muac* | Mid-upper arm circumference in millimetres |
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
#' A sample data from a community-based sentinel site in an anonymized location
#'
#' @description
#' Data was collected from community-based sentinel sites located across three 
#' provinces. Each provincial data set presents distinct data quality scenarios, 
#' requiring tailored prevalence analysis:
#' 
#' - *Province 1* has a MUAC-for-age z-score standard deviation and age ratio 
#' test rating of acceptability falling within range
#' - *Province 2* has age ratio rated as problematic but with an acceptable 
#' standard deviation of MUAC-for-age z-score
#' - *"Province 3* has both tests rated as problematic
#'
#' This sample data is useful to demonstrate the use of the prevalence functions 
#' on a multiple-domain survey data where variations in the rating of 
#' acceptability of the standard deviation exist, hence require different 
#' analytical approach for each domain to ensure accurate estimation.
#'
#' @format A tibble of 3,002 x 8.
#'
#' |**Variable** | **Description** |
#' | :--- | :---|
#' | *province* | Survey location |
#' | *cluster* | Primary sampling unit |
#' | *sex* | Sex; "m" = boys, "f" = girls |
#' | *age* | Calculated age in months with two decimal places |
#' | *muac* | Mid-upper arm circumference in millimetres |
#' | *edema* | Edema; "n" = no edema, "y" = with edema |
#' | *mfaz* | MUAC-for-age z-scores with 3 decimal places |
#' | *flag_mfaz* | Flagged MUAC-for-age z-score value; 1 = flagged, 0 = not flagged |
#'
#' @source Anonymous
#'
#' @examples
#' anthro.04
#'
#'
"anthro.04"


#'
#' A sample SMART survey data with weight-for-height z-score standard deviation 
#' rated as problematic
#'
#' @format A tibble with 303 rows and 6 columns.
#'
#' | **Variable** | **Description** |
#' | :--- | :---|
#' | *cluster* | Primary sampling unit |
#' | *sex* | Sex; "m" = boys, "f" = girls |
#' | *age* | Calculated age in months with two decimal places |
#' | *edema* | Edema, "n" = no edema, "y" = with edema |
#' | *wfhz* | MUAC-for-age z-scores with 3 decimal places |
#' | *flag_wfhz* | Flagged weight-for-height z-score value; 1 = flagged, 0 = not flagged |
#'
#' @source Anonymous
#'
#' @examples
#' wfhz.01
#'
#'
"wfhz.01"


#'
#' A sample mid-upper arm circumference (MUAC) screening data
#'
#' @format A tibble with 661 rows and 4 columns.
#'
#' |**Variable** | **Description** |
#' | :--- | :---|
#' | *sex* | Sex; "m" = boys, "f" = girls |
#' | *months* | Calculated age in months with two decimal places |
#' | *edema* | Edema, "n" = no edema, "y" = with edema |
#' | *muac* | Mid-upper arm circumference in millimetres |
#'
#' @source Anonymous
#'
#' @examples
#' mfaz.01
#'
"mfaz.01"

#'
#' A sample SMART survey data with mid-upper arm circumference measurements
#'
#' @format A tibble with 303 rows and 7 columns.
#'
#' |**Variable** | **Description** |
#' | :--- | :---|
#' | *cluster* | Primary sampling unit |
#' | *sex* | Sex; "m" = boys, "f" = girls |
#' | *age* | Calculated age in months with two decimal places |
#' | *edema* | Edema, "n" = no edema, "y" = with edema |
#' | *mfaz* | MUAC-for-age z-scores with 3 decimal places |
#' | *flag_mfaz* | Flagged MUAC-for-age z-score value. 1 = flagged, 0 = not flagged |
#'
#' @source Anonymous
#'
#' @examples
#' mfaz.02
#'
"mfaz.02"
