#' Provide details about individual events within a constellation
#'
#' A function that reads in multiple time series data frames for various events
#'  and builds indicator variables for each event. Individual events that occur
#'  within a specified number from a timestamp are flagged. The variables for
#'  each event can be populated with the time the event took place, a boolean
#'  variable (0 or 1) indicating whether or not the event took place, or the
#'  result of the variable at the time the event took place. 
#'
#' The user passes an arbitrary number of time series data frames and
#'  specifies a name and number of hours to search for each event. The user
#'  must also specify a variable to use to join the tables, and the time stamp
#'  variable. Finally, the user selects how to populate the individual event 
#'  variables.
#'
#' This function extends the constellate function to address a different set
#'  of questions, including: 1) at a specific timestamp, which events do and do
#'  not occur? 2) what is the sequence of events that trigger the constellation
#'  of events that I'm interested in? 3) What are the results of each criteria
#'  at the times that each criteria are met?
#' This function can be used to calculate risk scores at any measurement 
#'  timestamp by building a new variable after the function runs and returns
#'  the new data frame. The risk score can add up the crieteria from boolean
#'  values (e.g.
#'  \href{https://www.mdcalc.com/sirs-sepsis-septic-shock-criteria}{SIRS Criteria})
#'  or can be a linear combination of criteria (e.g.,
#'  \href{https://www.mdcalc.com/national-early-warning-score-news}{NEWS Score}).
#'
#' @param ... An arbitrary number of time series data frames that each include
#'  the columns 'join_key' and 'time_var'
#' @param criteria_names A vector of strings specifying the name of each
#'  event. The order of strings in the vector should align with the order of
#'  data frames passed in '...'.
#' @param window_hours A single numeric or vector of numerics specifying the
#'  number of hours to search for each event. The order of numerics in the
#'  vector should align with the order of data frames passed in '...'.
#' @param join_key A string name of the column to join all time series data
#'  frames
#' @param time_var A string name of the time stamp column in all time series
#'  data frames
#' @param value A string specifying the value to be entered within each 
#'  criteria column. Options include boolean (0 or 1, depdending on whether
#'  the criteria event occurred), the time of the criteria event, or the
#'  result stored within the criteria event. The default value is boolean.
#' @param result_var A string name of the value variable in all data frames.
#'  This argument should only be supplied if the "result" option is selected
#'  in the value argument.
#'
#' @return A data.frame, data.table with indicator variables for each event.
#'  The total number of rows is the unique number of time stamps for
#'  all combined measurements.
#'
#' @section Imported functions:
#' fastPOSIXct() from fasttime package and data.table syntax
#'
#' @section Errors:
#' This function returns errors for:
#' \itemize{
#'  \item missing arguments (value has a default argument and result_var is
#'    missing by default)
#'  \item passing a window_hours value that is not numeric
#'  \item passing join_key or time_var values that are not column names in all
#'    time series data frames
#'  \item passing an invalid number of criteria_names (must be equal to number
#'    of criteria event data frames)
#'  \item passing an invalid number of window_hours values (1 or the number of
#'    criteria event data frames).
#' }
#'
#' @examples
#' library(data.table)
#' temp <- as.data.table(vitals[VARIABLE == "TEMPERATURE"])
#' pulse <- as.data.table(vitals[VARIABLE == "PULSE"])
#' resp <- as.data.table(vitals[VARIABLE == "RESPIRATORY_RATE"])
#'
#' # Pass single window_hours
#' constellate_criteria(temp, pulse, resp, criteria_names = c("TEMPERATURE",
#'  "PULSE", "RESPIRATORY_RATE"), window_hours = 6, join_key = "PAT_ID",
#'  time_var = "RECORDED_TIME", value = "time")
#' # Pass vector for window_hours
#' constellate_criteria(temp, pulse, resp, criteria_names = c("TEMPERATURE",
#'  "PULSE", "RESPIRATORY_RATE"), window_hours = c(6,6,6), join_key = "PAT_ID",
#'  time_var = "RECORDED_TIME", value = "time")
#' # Show the value of each criteria at the time the event occurs
#' constellate_criteria(temp, pulse, resp, criteria_names = c("TEMPERATURE",
#'  "PULSE", "RESPIRATORY_RATE"), window_hours = c(6,6,6), join_key =
#'  "PAT_ID", time_var = "RECORDED_TIME", value = "result", result_var = "VALUE")
#'
#' @export

constellate_criteria <- function(..., criteria_names, window_hours, join_key,
  time_var, value = c("boolean", "time", "result"), result_var = NULL) {
  # Build list
  criteria_list <- list(...)

  ############ Error handling -------------------------------------------------
  # Missing arguments
  if (length(criteria_list) < 2) {
    stop("Need to pass at least two time series data frames")
  }
  if (missing(window_hours)) stop("Need to specify window_hours")
  if (missing(join_key)) stop("Need to specify join_key")
  if (missing(time_var)) stop("Need to specify time_var")
  if (missing(criteria_names)) stop("Need to provide criteria_names")

  # criteria_names must be strings
  for (i in criteria_names) {
    if (!is.character(i)) stop("All criteria_names must be strings")
  }

  # window_hours must be numeric and greater than 0
  for (i in window_hours) {
    if (!is.numeric(i)) stop("All window_hours must be numeric")
  }

  # Ensure that first argument is data frames
  for (i in seq_len(length(criteria_list))) {
    if (!is.data.frame(criteria_list[[i]])) {
      stop("Need to pass only data frames in first argument")
    }
  }

  # Ensure join_key and time_var are variables names in all data frames
  for (i in seq_len(length(criteria_list))) {
    if (sum(grepl(join_key, names(criteria_list[[i]]))) == 0) {
      stop("'join_key' is not a column name in all time series data frames")
    }
    if (sum(grepl(time_var, names(criteria_list[[i]]))) == 0) {
      stop("'time_var' is not a column name in all time series data frames")
    }
  }

  # Value argument not from set of options
  value <- match.arg(value)

  # Must pass result_var if select result option
  if (value == "result" & missing(result_var)) {
    stop("Need to specify result_var")
  }

  # If result_var is supplied, ensure it is in all data frames
  if (!missing(result_var)) {
    for (i in seq_len(length(criteria_list))) {
      if (sum(grepl(result_var, names(criteria_list[[i]]))) == 0) {
        stop("'result_var' is not a column name in all time series data frames")
      }
    }
  }

  # Number of names provided matches number of data frames passed
  if (length(criteria_list) != length(criteria_names)) {
    stop(paste0("Need to pass a name for each criteria data frame. The number",
      " of data frames does not equal the number of names."))
  }

  # Number of hour windows matches number of data frames passed
  if (length(criteria_list) != length(window_hours) &
      length(window_hours) != 1) {
    stop(paste0("Need to pass a single window hour length for all criteria",
      " data frames or a window hour length for each criteria data frame."))
  }

  ############ Prep data for joins --------------------------------------------
  for (i in seq_len(length(criteria_list))) {
    # Ensure class of time_var
    set(criteria_list[[i]], j = time_var,
      value = fastPOSIXct(criteria_list[[i]][[time_var]], tz = "GMT"))

    # Create criteria variable based on value argument
    if (value == "time") {
      # Subset data frames
      criteria_list[[i]] <- data.table(
        criteria_list[[i]][, c(join_key, time_var), with = FALSE]
      )

      criteria_list[[i]][, c(criteria_names[i]) := get(time_var)]
    } else if (value == "boolean") {
      # Subset data frames
      criteria_list[[i]] <- data.table(
        criteria_list[[i]][, c(join_key, time_var), with = FALSE]
      )

      criteria_list[[i]][, c(criteria_names[i]) := 1]
    } else if (value == "result") {
      # Subset data frames
      criteria_list[[i]] <- data.table(
        criteria_list[[i]][, c(join_key, time_var, result_var), with = FALSE]
      )

      setnames(criteria_list[[i]], result_var, criteria_names[i])
    }

    # Set keys
    setkeyv(criteria_list[[i]], c(join_key, time_var))
  }

  ########### Define parameters -----------------------------------------------
  # Set time windows
  join_window <- 60 * 60 * window_hours

  # Build empty data set to store events
  event_df <- data.table(NULL)

  ############ Rolling joins --------------------------------------------------
  ##### Consider each criteria as a final event
  for (i in seq_len(length(criteria_list))) {

    for (j in setdiff(seq_len(length(criteria_list)), i)) {
      # Perform rolling join with either same window for all or drawing
        # in each window hour separately
      if (length(window_hours) == 1){
        event_subset <- criteria_list[[j]][criteria_list[[i]],
          roll = join_window]
      } else {
        event_subset <- criteria_list[[j]][criteria_list[[i]],
          roll = join_window[j]]
      }

      # Update table to join
      criteria_list[[i]] <- event_subset
    }

    # Eliminate added columns from raw event data
    criteria_list[[i]] <- criteria_list[[i]][, c(join_key, time_var,
      criteria_names[i]), with = FALSE]

    # Normalize column order
    setcolorder(event_subset,c(join_key, time_var, criteria_names))

    # Append file to event table
    event_df <- rbind(event_df, event_subset)
  }

  ############ Clean before writing out ---------------------------------------
  # Order by join key and time variable
  setkeyv(event_df, c(join_key, time_var))

  # Drop duplicate rows
  event_df <- unique(event_df)

  # Replace NAs with 0s for boolean option
  if (value == "boolean") {
    for (col in names(event_df)) {
      set(event_df, which(is.na(event_df[[col]])), col, 0)
    }
  }

  ############ Return event time table ----------------------------------------
  return(event_df)
}