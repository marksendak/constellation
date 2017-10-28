#' Synthesized vital sign measurements for cohort of 100 synthetic patients
#'
#' A dataset containing 35,146 lab results for 100 unique synthetic patients.
#'  There are 4 unique vital signs, including systolic blood pressure, heart
#'  rate (pulse), respiratory rate, and temperature. Patient-level sampling
#'  rate, mean numeric value, standard deviation, and number of measurements
#'  per patient follow patterns observed in real electronic health record data.
#'  This dataset is included to demonstrate sepsis identification.
#'
#' @name vitals
#' @format A data frame with 35146 rows and 4 variables:
#' \describe{
#'  \item{PAT_ID}{patient identification number, randomly generated integer}
#'  \item{RECORDED_TIME}{recorded datetime of vital sign measurement,
#'  character}
#'  \item{VALUE}{vital sign measurement value, numeric}
#'  \item{VARIABLE}{categorical variable specifying the vital sign name,
#'  character}
#' }
#'
#' @examples
#' \dontrun{
#'  vitals
#' }
#'
#' @source Randomly synthesized patient data
"vitals"
