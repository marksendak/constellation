#' Synthesized lab results for cohort of 100 synthetic patients
#'
#' A dataset containing 3,150 lab results for 96 unique synthetic patients.
#'  There are 6 unique labs, including platelets, serum creatinine,
#'  international normalized ratio (INR), white blood count (WBC), serum
#'  lactate, and serum bilirubin. Patient-level sampling rate, mean numeric
#'  value, standard deviation, and number of measurements per patient follow
#'  patterns observed in real electronic health record data. This dataset is
#'  included to demonstrate sepsis identification.
#'
#' @name labs
#' @format A data frame with 3150 rows and 4 variables:
#' \describe{
#'  \item{PAT_ID}{patient identification number, randomly generated integer}
#'  \item{RECORDED_TIME}{recorded datetime of lab measurement, character}
#'  \item{VALUE}{lab result value, numeric}
#'  \item{VARIABLE}{categorical variable specifying the lab name, character}
#' }
#'
#' @examples
#' \dontrun{
#'  labs
#' }
#'
#' @source Randomly synthesized patient data
"labs"
