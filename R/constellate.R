#' Identify when a constellation of events occur
#'
#' A function that reads in multiple time series data frames and calculates
#'  instances when a constellation of events occur.
#' The user must specify the number of hours over which each event must take
#'  place, a variable to use to join the tables, and the time stamp variable.
#'  The timestamps variable in every data frame must be POSIXct class. In 
#'  addition, the user must specify the event name and whether to keep all
#'  instances that events occur, or only the first or last instance. This
#'  function can ingest an arbitrary number of data frames with longitudinal
#'  time series data.
#'
#' @param ... An arbitrary number of time series data frames that each include
#'  the columns 'join_key' and 'time_var'
#' @param window_hours A single numeric or vector of numerics specifying the
#'  number of hours to search for each event. The order of numerics in the
#'  vector should align with the order of data frames passed in '...'.
#' @param join_key A string name of the column to join all time series data
#'  frames
#' @param time_var A string name of the time stamp column in all time series
#'  data frames. The class of time_var must be POSIXct in all data frames.
#' @param event_name A string name for events across the time series data
#'  frames
#' @param mult A string specifying whether to return the first, last, or all
#'  instance(s) with a default value of all
#'
#' @return A data.frame, data.table with time stamps of qualifying events.
#'
#' @section Imported functions:
#' general data.table syntax
#'
#' @section Errors:
#' This function returns errors for:
#' \itemize{
#'  \item missing arguments (no arguments have defaults)
#'  \item passing an invalid mult value
#'  \item passing arguments with invalid classes (window_hours must be numeric
#'    and event_name must be a string)
#'  \item passing join_key or time_var values that are not column names in all
#'    time series data frames
#'  \item passing an invalid number of window_hours values (1 or the number of
#'  event data frames).
#' }
#'
#' @examples
#' library(data.table)
#' temp <- as.data.table(vitals[VARIABLE == "TEMPERATURE"])
#' pulse <- as.data.table(vitals[VARIABLE == "PULSE"])
#' resp <- as.data.table(vitals[VARIABLE == "RESPIRATORY_RATE"])
#' wbc <- as.data.table(labs[VARIABLE == "WBC"])

#' temp[, RECORDED_TIME := as.POSIXct(RECORDED_TIME,
#'   format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")]
#' pulse[, RECORDED_TIME := as.POSIXct(RECORDED_TIME,
#'   format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")]
#' resp[, RECORDED_TIME := as.POSIXct(RECORDED_TIME,
#'   format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")]
#' wbc[, RECORDED_TIME := as.POSIXct(RECORDED_TIME,
#'   format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")]
#'
#' # Pass single time window for all time series data frames
#' # Subset first event
#' constellate(temp, pulse, resp, window_hours = 6, join_key = "PAT_ID",
#'  time_var = "RECORDED_TIME", event_name = "sirs_vitals", mult = "first")
#' # Pass different time window for each time series data frame
#' # Subset first event
#' constellate(temp, pulse, resp, wbc, window_hours = c(6,6,6,24),
#'  join_key = "PAT_ID", time_var = "RECORDED_TIME",
#'  event_name = "SEPSIS", mult = "first")
#' # Pass different time window for each time series data frame
#' # Identify all events
#' constellate(temp, pulse, resp, wbc, window_hours = c(6,6,6,24),
#'  join_key = "PAT_ID", time_var = "RECORDED_TIME",
#'  event_name = "SEPSIS", mult = "all")
#'
#' @export

constellate <- function(..., window_hours, join_key, time_var, event_name,
  mult = c("all", "first", "last")) {

  # Build list
  criteria_list <- list(...)

  ########## Error handling --------------------------------------------------
  # Missing arguments
  if (length(criteria_list) < 2) {
      stop("Need to pass at least two time series data frames")
  }
  if (missing(window_hours)) stop("Need to specify window hours")
  if (missing(join_key)) stop("Need to specify join key")
  if (missing(time_var)) stop("Need to specify time variable")
  if (missing(event_name)) stop("Need to specify an event name")

  # Mult argument not from set of options
  mult <- match.arg(mult)

  # Ensure that first argument is data frames
  for (i in seq_len(length(criteria_list))) {
    if (!is.data.frame(criteria_list[[i]])) {
      stop("Need to pass only data frames in first argument")
    }
  }

  # event_name must be string
  if (!is.character(event_name)) {
    stop("'event_name' must be a character string")
  }

  # window_hours must be numeric
  for (i in window_hours) {
    if (!is.numeric(i)) stop(" All window_hours must be numeric")
  }

  # Number of hour windows is 1 or matches number of data frames passed
  if (length(criteria_list) != length(window_hours) &
    length(window_hours) != 1) {
    stop(paste0("Need to pass a single window hour length for all criteria",
      " data frames or a window hour length for each criteria data frame."))
  }

  # Ensure join_key and time_var are variable names in all passed data frames
  for (i in seq_len(length(criteria_list))) {
    if (sum(grepl(join_key, names(criteria_list[[i]]))) == 0) {
      stop("'join_key' is not a column name in all time series data frames")
    }
    if (sum(grepl(time_var, names(criteria_list[[i]]))) == 0) {
      stop("'time_var' is not a column name in all time series data frames")
    }
  }

  # Ensure time_var variable in all data frames is class POSIXct
  for (i in seq_len(length(criteria_list))) {
    if (!("POSIXct" %in% class(criteria_list[[i]][[time_var]]))) {
      stop(paste0("'time_var' column in all time series data frames",
        " must be POSIXct class"))
    }
  }

  ########### Prep data for joins ---------------------------------------------
  for (i in seq_len(length(criteria_list))) {
    # Subset data frames
    criteria_list[[i]] <- data.table(
        criteria_list[[i]][, c(join_key, time_var), with = FALSE]
      )

    # Create criteria variable
    criteria_list[[i]][, c(paste0("CRITERIA", "_", i)) :=
      paste0("CRITERIA", "_", i)]

    # Set keys
    setkeyv(criteria_list[[i]], c(join_key, time_var))
  }

  ########### Define parameters -----------------------------------------------
  # Set time windows
  join_window <- 60 * 60 * window_hours

  # Build empty data set to store events
  event_df <- data.table(NULL)

  ########### Rolling joins ---------------------------------------------------
  #### Consider each criteria as a final event
  for (i in seq_len(length(criteria_list))) {

    for (j in setdiff(seq_len(length(criteria_list)), i)) {
      # Perform rolling join with either same window for all or drawing in each
        # window hour separately
      if (length(window_hours) == 1) {
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
      paste0("CRITERIA_", i)), with = FALSE]

    # Normalize column order
    setcolorder(event_subset, c(join_key, time_var, paste0("CRITERIA_",
      c(1:length(criteria_list)))))

    # Append file to event table
    event_df <- rbind(event_df, event_subset)
  }

  ########### Subset rows and columns ---------------------------------------
  # Drop observations with missing criteria
  for (i in seq_len(length(criteria_list))) {
    event_df <- subset(event_df, !is.na(get(paste0("CRITERIA", "_", i))))
  }

  # Drop criteria variables
  event_df[, c(paste0("CRITERIA_", c(1:length(criteria_list)))) := NULL]

  # Change time variable to time of event
  setnames(event_df, time_var, paste0(toupper(event_name), "_TIME"))

  # Order by encounter and then time of event
  setkeyv(event_df, c(join_key, paste0(toupper(event_name), "_TIME")))

  # Drop duplicate rows
  event_df <- unique(event_df)

  ########### Subset to first, last, or all rows per patient
  if (mult == "first") {
    event_df <- event_df[, .SD[1], by = join_key]
  } else if (mult == "last") {
    event_df <- event_df[, .SD[.N], by = join_key]
  }

  ########### Return event time table ---------------------------------------
  return(event_df)
}