#' Identify changes in a value over time
#'
#' A function that reads in a time series data frame along with a specified
#'  value change and identifies instances where the value change occurs.
#' The user must specify the number of hours over which the value change must
#'  take place, the magnitude and direction of the value change, a variable to
#'  use to join the table to itself, the time stamp variable, and the value
#'  variable. The timestamps variable in every data frame must be POSIXct 
#'  class. The user must also specify whether to keep all instances that the
#'  value change occurs, or only the first or last instance.
#' This function must be used carefully, because certain types of arguments
#'  will cause the function to output a data frame with nrow(data)^2, where
#'  'data' is the input data. More specifically, if the user is trying to
#'  detect small variations in a value over a large period of time, the size
#'  of input 'data' should be limited.
#'
#' @param data A time series data frame that includes the columns 'join_key',
#'  'time_var', and 'value_var'
#' @param value A numeric value specifying the magnitude of change to identify
#' @param direction A string value specifying whether to identify changes in
#'  the value up (an increase), down (a decrease), or all (both). The default
#'  value is all.
#' @param window_hours A numeric value specifying the number of hours to search
#'  for the value change
#' @param join_key A string name of the column to join the time series data
#'  frame to itself. In other words, the primary key to the 'data' argument.
#' @param time_var A string name of the time stamp column in all time series
#'  data frames. The class of time_var must be POSIXct in all data frames.
#' @param value_var A string name of the value variable column in the time
#'  series data frame
#' @param mult A string specifying whether to return the first, last, or all
#'  instance(s) of the value change with a default value of all
#'
#' @return A data.frame, data.table with time stamps of value changes over time
#'  along with values and time stamps for prior measurements
#'
#' @section Imported functions:
#' foverlaps() from data.table and general data.table syntax
#'
#' @section Errors:
#' This function returns errors for:
#' \itemize{
#'  \item missing arguments (no arguments have defaults)
#'  \item passing an invalid direction or mult value
#'  \item passing arguments with invalid classes (data must be a data frame,
#'    value must be numeric, and window_hours must be numeric)
#'  \item passing join_key, time_var, or value_var values that are not column
#'    names in input data frames
#' }
#'
#' @examples
#' library(data.table)
#' systolic_bp <- as.data.table(vitals[VARIABLE == "SYSTOLIC_BP"])
#'
#' systolic_bp[, RECORDED_TIME := as.POSIXct(RECORDED_TIME,
#'   format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")]
#'
#' # Identify all instances of a drop of 40 over 6 hours
#' value_change(systolic_bp, value = 40, direction = "down", window_hours = 6,
#'  join_key = "PAT_ID", time_var = "RECORDED_TIME",
#'  value_var = "VALUE", mult = "all")
#' # Identify first instance of a drop of 40 over 6 hours
#' value_change(systolic_bp, value = 40, direction = "down", window_hours = 6,
#'  join_key = "PAT_ID", time_var = "RECORDED_TIME",
#'  value_var = "VALUE", mult = "first")
#' # Identify last instance of a drop of 40 over 6 hours
#' value_change(systolic_bp, value = 40, direction = "down", window_hours = 6,
#'  join_key = "PAT_ID", time_var = "RECORDED_TIME",
#'  value_var = "VALUE", mult = "last")
#' # Identify all instances of drops and increases of 40 over 6 hours
#' value_change(systolic_bp, value = 40, direction = "all", window_hours = 6,
#'  join_key = "PAT_ID", time_var = "RECORDED_TIME",
#'  value_var = "VALUE", mult = "all")
#'
#' @export

value_change <- function(data, value, direction = c("all", "up", "down"),
  window_hours, join_key, time_var, value_var, mult = c("all", "first",
  "last")) {

  ########## Error handling ---------------------------------------------------
  # Missing arguments
  if (missing(data)) stop("Need to pass data frame")
  if (missing(value)) stop("Need to specify change value")
  if (missing(window_hours)) stop("Need to specify window hours")
  if (missing(join_key)) stop("Need to specify join key")
  if (missing(time_var)) stop("Need to specify time variable")
  if (missing(value_var)) stop("Need to specify value variable")

  # Direction and mult not from set of options
  direction <- match.arg(direction)
  mult <- match.arg(mult)

  # Confirm argument classes
  if (!is.data.frame(data)) stop("'data' must be a data.frame")
  if (!is.numeric(value)) stop("'value' must be numeric")
  if (!is.numeric(window_hours)) stop("'window_hours' must be numeric")

  # Ensure join_key, time_var, and value_var are variables in data
  if (sum(grepl(join_key, names(data))) == 0) {
    stop("'join_key' is not a column name in data")
  }
  if (sum(grepl(time_var, names(data))) == 0) {
    stop("'time_var' is not a column name in data")
  }
  if (sum(grepl(value_var, names(data))) == 0) {
    stop("'value_var' is not a column name in data")
  }

  # Ensure time_var variable in all data frames is class POSIXct
  if (!("POSIXct" %in% class(data[[time_var]]))) {
      stop(paste0("'time_var' column must be POSIXct class"))
  }

  ########## Prep data for joins ----------------------------------------------
  # Ensure classes
  data <- data.table(data)
  set(data, j = value_var, value = as.numeric(data[[value_var]]))

  # Subset data
  data <- data[, c(join_key, time_var, value_var), with = FALSE]

  # Upper case time_var, value_var
  setnames(data, time_var, toupper(time_var))
  time_var <- toupper(time_var)
  setnames(data, value_var, toupper(value_var))
  value_var <- toupper(value_var)

  # Set keys
  setkeyv(data, c(join_key, time_var))

  # Subset to possible encounters that show necessary variation
  V1 = NULL
  event_subset <- data[, (max(get(value_var)) - min(get(value_var))),
    by = join_key][V1 >= value][[join_key]]
  data <- data[get(join_key) %in% event_subset]

  # Make copy for join
  prior_data <- copy(data)

  # Normalize column names
  setnames(data, c(time_var, value_var), c(paste0("CURRENT_", time_var),
    paste0("CURRENT_", value_var)))
  setnames(prior_data, c(time_var, value_var), c(paste0("PRIOR_", time_var),
    paste0("PRIOR_", value_var)))

  # Add end time to start table (end time = start time + window_hours)
  prior_data[, paste0("CURRENT_", time_var) := (get(paste0("PRIOR_", time_var))
   + (window_hours * 60 * 60))]
  setcolorder(prior_data, c(join_key, paste0("PRIOR_", time_var),
    paste0("CURRENT_", time_var), paste0("PRIOR_", value_var)))

  # Add start time to end table (start time = end time)
  data[, paste0("PRIOR_", time_var) := get(paste0("CURRENT_", time_var))]
  setcolorder(data, c(join_key, paste0("PRIOR_", time_var),
    paste0("CURRENT_", time_var), paste0("CURRENT_", value_var)))

  # Set keys
  setkeyv(data, c(join_key, paste0("PRIOR_", time_var),
    paste0("CURRENT_", time_var)))
  setkeyv(prior_data, c(join_key, paste0("PRIOR_", time_var),
    paste0("CURRENT_", time_var)))

  ########## Overlap join and post-process ------------------------------------
  # Overlap join
  join_data <- foverlaps(data, prior_data)

  # Drop columns
  join_data[,c(paste0("CURRENT_", time_var),
    paste0("i.",paste0("PRIOR_", time_var))) := NULL]

  # Fix column names
  setnames(join_data, paste0("i.", paste0("CURRENT_", time_var)),
    paste0("CURRENT_", time_var))

  # Drop rows where measurement time are the same
  join_data <- join_data[get(paste0("PRIOR_", time_var)) !=
    get(paste0("CURRENT_", time_var))]

  # Order by join_key and end time
  setkeyv(join_data, c(join_key, paste0("CURRENT_", time_var)))

  # Identify value changes
  if (direction == "up") {
    join_data <- join_data[get(paste0("PRIOR_", value_var)) -
      get(paste0("CURRENT_", value_var)) <= - value]
  }
  else if (direction == "down") {
    join_data <- join_data[get(paste0("PRIOR_", value_var)) -
      get(paste0("CURRENT_", value_var)) >= value]
  }
  else {
    join_data <- join_data[abs(get(paste0("PRIOR_", value_var)) -
      get(paste0("CURRENT_", value_var))) >= value]
  }

  ########## Subset events ----------------------------------------------------
  # Subset events
  if (mult == "first") {
    join_data <- join_data[, .SD[1], by = join_key]
  }
  else if (mult == "last") {
    join_data <- join_data[, .SD[.N], by = join_key]
  }

  ########## Return table -----------------------------------------------------
  return(join_data)
}
