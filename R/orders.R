#' Synthesized blood culture orders for cohort of 100 synthetic patients
#'
#' A dataset containing 59 blood culture orders for 27 unique synthetic
#'  patients. There is only 1 order for blood cultures. Patient-level sampling
#'  rate, mean numeric value, standard deviation, and number of measurements
#'  per patient follow patterns observed in real electronic health record data.
#'  This dataset is included to demonstrate sepsis identification.
#'
#' @name orders
#' @format A data frame with 59 rows and 3 variables:
#' \describe{
#'  \item{PAT_ID}{patient identification number, randomly generated integer}
#'  \item{ORDER_TIME}{order datetime of blood culture, character}
#'  \item{VARIABLE}{categorical variable specifying the blood culture order
#'  name, character}
#' }
#'
#' @examples
#' \dontrun{
#'  orders
#' }
#'
#' @source Randomly synthesized patient data
"orders"
