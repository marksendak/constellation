#' Identify incident events separated by a minimum time window
#'
#' A function that reads in a time series data frame along with a specified
#'  time window and identifies incident events that are separated in time.
#' The user must specify the number of hours over which events are considered
#'  to be the same episode, the time stamp variable, and an optional variable
#'  to group episodes. This function was motivated by examples where there may
#'  be multiple observations of the same illness episode combined with
#'  observations of distinct illness episodes and there is a need to
#'  distinguish between episodes. This function assumes that the duration of
#'  an episode is non-zero. If every non-equal instant is a distinct episode,
#'  there is no need to use this function.
#' Two ways to distinguish between episodes with non-zero duration over time
#'  are: (1) bucket observations over a pre-fixed time frame that is applied to
#'  all observation; (2) bucket observations over a fixed window of time. An
#'  example of (1) is considering observations in the same month to be the same
#'  episode and observations in different months to be distinct episodes. The
#'  incident() function addresses (2) by specifying a time window and
#'  identifying the first observation of each episode. The use can also specify
#'  a 'join_key' variable (person, encounter, etc.) to group episodes.
#'  The 'window_hours' argument serves as the lower bound to separate
#'  observations that are considered the same episode versus distinct episodes.
#'
#' @param data A time series data frame that includes the columns 'join_key'
#'  and 'time_var'
#' @param window_hours A numeric value specifying the number of hours to
#'  separate contiguous episodes and distinct episodes
#' @param time_var A string name of the time stamp column in the time series
#'  data frame
#' @param join_key An optional string name of the column to group observations
#'
#' @return A data.frame, data.table with the time stamps of distinct, incident
#'  episodes separated by at least 'window_hours'
#'
#' @section Imported functions:
#' general data.table syntax
#'
#' @section Errors:
#' This function returns errors for:
#' \itemize{
#'  \item missing arguments (join_key is missing by default)
#'  \item passing arguments with invalid classes (data must be a data frame
#'    and window_hours must be numeric)
#'  \item passing join_key or time_var values that are not column
#'    names in input data
#'  \item passing time_var column in data that is not POSIXct class
#' }
#'
#' @examples
#' library(data.table)
#' systolic_bp <- as.data.table(vitals[VARIABLE == "SYSTOLIC_BP"])
#' systolic_bp[, RECORDED_TIME := as.POSIXct(RECORDED_TIME,
#'   format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")]
#'
#' # Identify systolic blood pressure measurements for each patient that are
#'  # separated by at least 24 hours
#' incidents(systolic_bp, window_hours = 24, join_key = "PAT_ID",
#'  time_var = "RECORDED_TIME")
#'
#' # Identify systolic blood pressure measurements that are separated by at
#'  # least 24 hours
#' incidents(systolic_bp, window_hours = 24, time_var = "RECORDED_TIME")
#'
#' @export

incidents <- function(data, window_hours, time_var, join_key = NULL) {

  ########## Error handling ---------------------------------------------------
  # Missing arguments
  if (missing(data)) stop("Need to pass data frame")
  if (missing(window_hours)) stop("Need to specify window hours")
  if (missing(time_var)) stop("Need to specify time variable")

  # Confirm argument classes
  if (!is.data.frame(data)) stop("'data' must be a data.frame")
  if (!is.numeric(window_hours)) stop("'window_hours' must be numeric")

  # Ensure join_key and time_var are variables in data
  if (!missing(join_key)) {
    if (sum(grepl(join_key, names(data))) == 0) {
      stop("'join_key' is not a column name in data")
    }
  }
  if (sum(grepl(time_var, names(data))) == 0) {
    stop("'time_var' is not a column name in data")
  }

  # Confirm class of time_var
  if (!("POSIXct" %in% class(data[[time_var]]))) {
    stop("'time_var' column in data must be POSIXct class")
  }

  ########## Prep data for loop -----------------------------------------------
  # Ensure classes
  data <- data.table(data)

  # Drop duplicate timestamps
  data <- unique(data)

  # Build incident df
  incident <- data.table(NULL)

  # Set keys
  if (missing(join_key)) {
    setkeyv(data, time_var)
  } else {
    setkeyv(data, c(join_key, time_var))
  }

  # Loop through events until gone through all events
  while(nrow(data) > 0) {
    # Empty placeholders for new variables
    V1 = NULL
    KEEP = NULL

    # Drop events solely by timestamp
    if (missing(join_key)) {
      # Add first event to incident table
      incident <- rbind(incident, data[1])

      # Remove observations within window_hours from data
      data <- data[, KEEP := get(time_var) >= (get(time_var)[1] +
        window_hours*60*60)][KEEP == "TRUE"][, KEEP := NULL]
    } else {
      # Add first event by join_key to incident table
      incident <- rbind(incident, data[, .SD[1], by = join_key])

      # Remove observations within window_hours from data by join_key
      data <- data[, KEEP := get(time_var) >= (get(time_var)[1] +
        window_hours*60*60), by = join_key][KEEP == "TRUE"][, KEEP := NULL]
    }
  }

  # Set keys for incident table
  if (missing(join_key)) {
    setkeyv(incident, time_var)
  } else {
    setkeyv(incident, c(join_key, time_var))
  }

  # Drop duplicates
  incident <- unique(incident)

  # return incident events
  return(incident)
}
