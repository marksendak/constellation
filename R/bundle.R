#' Identify bundle items that occur around a given event
#'
#' A function that reads in a data frame of incident events along with multiple
#'  time series data frames of bundle items and calculates whether or not each
#'  bundle item occurs within a defined time window around the incident event.
#' The user must provide names for each bundle item, define the time window
#'  around the incident events to consider, a name for the incident event, and
#'  variables to use to join the different tables. Lastly, the user can specify
#'  whether to return all instances that each bundle item occurs around the
#'  incident event, or whether to pull only the first or last instant for each
#'  bundle item.
#' All time series data frames must contain columns for joining the tables (
#'  join_key) and time stamps (time_var). The time_var column must be class 
#'  POSIXct in all the data frames.
#' This function can ingest an arbitrary number of data frames for different
#'  bundle items around an incident event.
#'
#' @param events A time series data frame of incident events. The bundle events
#'  are searched for around a given time window of these events. The events
#'  data frame must include the columns 'join_key' and 'time_var'
#' @param ... An arbitrary number of time series data frames that each include
#'  the columns 'join_key' and 'time_var'. Each data frame consists of a bundle
#'  item that is important to find around the specified events.
#' @param bundle_names A vector of strings specifying the name of each
#'  event. The order of strings in the vector should align with the order of
#'  data frames passed in '...'.
#' @param window_hours_pre A single numeric or vector of numerics speficying
#'. the number of hours before the events in the events data frame that each
#'  bundle item is considered relevant. If a single numeric is passed, that
#'  time window before the events is applied to all bundle items.
#' @param window_hours_post A single numeric or vector of numerics speficying
#'. the number of hours after the events in the events data frame that each
#'  bundle item is considered relevant. If a single numeric is passed, that
#'  time window after the events is applied to all bundle items.
#' @param join_key A string name of the column to join all time series data
#'  frames
#' @param time_var A string name of the time stamp column in all time series
#'  data frames. The class of time_var must be POSIXct in all data frames.
#' @param event_name A string name of the events in the events data frame
#' @param mult A string specifying whether to return the first, last, or all
#'  instance(s) of every bundle item occurring within the specified time window
#'  of events. The default value is all.
#'  
#' @return A data.frame, data.table with a time stamp for every event of
#'  interest, columns for the start and end of the time window of interest,
#'  and columns for every bundle item. The value in bundle item columns is
#'  the timestamp (time_var) that the bundle item is observed within the given
#'  window.
#' 
#' @section Imported functions:
#' foverlaps() from data.table and general data.table syntax
#' 
#' @section Errors:
#' This function returns errors for:
#' \itemize{
#'  \item missing arguments (only the mult argument has a default value)
#'  \item passing arguments with invalid classes (events and bundle items must
#'    be data frames, bundle_names must be a string, window_hours_pre and
#'    window_hours_post must be numerics, and event_name must be a string)
#'  \item passing an invalid mult value
#'  \item passing join_key or time_var values that are not column names in all
#'    time series data frames
#'  \item passing an invalid number of window_hours_pre or window_hours_post
#'    values (1 or the number of bundle data frames).
#' }
#' 
#' @examples
#' library(data.table)
#' temp <- as.data.table(vitals[VARIABLE == "TEMPERATURE"])
#' pulse <- as.data.table(vitals[VARIABLE == "PULSE"])
#' resp <- as.data.table(vitals[VARIABLE == "RESPIRATORY_RATE"])
#'
#' temp[, RECORDED_TIME := as.POSIXct(RECORDED_TIME,
#'   format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")]
#' pulse[, RECORDED_TIME := as.POSIXct(RECORDED_TIME,
#'   format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")]
#' resp[, RECORDED_TIME := as.POSIXct(RECORDED_TIME,
#'   format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")]
#'
#' # Pass single window_hours_pre
#' # All instances of bundle items within time window of event
#' bundle(temp, pulse, resp,
#'     bundle_names = c("PLATELETS", "INR"), window_hours_pre = 24,
#'     window_hours_post = c(6, 6), join_key = "PAT_ID",
#'     time_var = "RECORDED_TIME", event_name = "CREATININE", mult = "all")
#' # Pass different window_hours_pre for each bundle time series data frame
#' # All instances of bundle items within time window of event
#' bundle(temp, pulse, resp,
#'     bundle_names = c("PLATELETS", "INR"), window_hours_pre = c(24, 12),
#'     window_hours_post = c(6, 6), join_key = "PAT_ID",
#'     time_var = "RECORDED_TIME", event_name = "CREATININE", mult = "all")
#' # Pass different window_hours_pre for each bundle time series data frame
#' # First instance of each bundle item within time window of event
#' bundle(temp, pulse, resp,
#'     bundle_names = c("PLATELETS", "INR"), window_hours_pre = c(24, 12),
#'     window_hours_post = c(6, 6), join_key = "PAT_ID",
#'     time_var = "RECORDED_TIME", event_name = "CREATININE", mult = "first")
#' # Pass different window_hours_pre for each bundle time series data frame
#' # Last instance of each bundle item within time window of event
#' bundle(temp, pulse, resp,
#'     bundle_names = c("PLATELETS", "INR"), window_hours_pre = c(24, 12),
#'     window_hours_post = c(6, 6), join_key = "PAT_ID",
#'     time_var = "RECORDED_TIME", event_name = "CREATININE", mult = "last")
#' 
#' @export

bundle <- function(events, ..., bundle_names, window_hours_pre, 
  window_hours_post, join_key, time_var, event_name, mult = c("all", "first",
  "last")) {
  ## Build list
  bundle_list <- list(...)

  ########## Error handling --------------------------------------------------
  # Missing arguments
  if (length(bundle_list) < 1) {
      stop("Need to pass at least one bundle item data frame")
  }
  if (missing(bundle_names)) stop("Need to provide bundle names")
  if (missing(window_hours_pre)) {
    stop("Need to specify window hours before the event")
  }
  if (missing(window_hours_post)) {
    stop("Need to specify window hours after the event")
  }
  if (missing(join_key)) stop("Need to specify join key")
  if (missing(time_var)) stop("Need to specify time variable")
  if (missing(event_name)) stop("Need to specify an event name")

  # Mult not from set of options
  mult <- match.arg(mult)

  # events must be a data frame
  if (!is.data.frame(events)) stop("Need to pass data frame in first argument")

  # Ensure that all bundle items are data frames
  for (i in seq_len(length(bundle_list))) {
    if (!is.data.frame(bundle_list[[i]])) {
      stop("Need to pass only data frames as bundle items")
    }
  }

  # bundle_names must be strings
  for (i in bundle_names) {
    if (!is.character(i)) stop("All bundle_names must be strings")
  }

  # window_hours_pre must be numeric
  for (i in window_hours_pre) {
    if (!is.numeric(i)) stop("All window_hours_pre must be numeric")
  }

  # window_hours_post must be numeric
  for (i in window_hours_post) {
    if (!is.numeric(i)) stop("All window_hours_post must be numeric")
  }

  # event_name must be string
  if (!is.character(event_name)) {
    stop("'event_name' must be a character string")
  }
  
  # Number of window_hours_pre is 1 or matches number of data frames passed
  if (length(bundle_list) != length(window_hours_pre) &
    length(window_hours_pre) != 1) {
    stop(paste0("Need to pass a single 'window_hours_pre' value for all",
      " bundle data frames or a 'window_hours_pre' value for each bundle data",
      " frame."))
  }

  # Number of window_hours_pre is 1 or matches number of data frames passed
  if (length(bundle_list) != length(window_hours_post) &
    length(window_hours_post) != 1) {
    stop(paste0("Need to pass a single 'window_hours_post' value for all",
      " bundle data frames or a 'window_hours_post' value for each bundle",
      " data frame."))
  }

  # Ensure join_key and time_var are variable names in bundle data frames
  for (i in seq_len(length(bundle_list))) {
    if (sum(grepl(join_key, names(bundle_list[[i]]))) == 0) {
      stop("'join_key' is not a column name in all time series data frames")
    }
    if (sum(grepl(time_var, names(bundle_list[[i]]))) == 0) {
      stop("'time_var' is not a column name in all time series data frames")
    }
  }

  # Ensure join_key and time_var are variable names in events data frame
  if (!(join_key %in% names(events))) {
    stop("'join_key' is not a column name in all time series data frames")
  }
  if (!(time_var %in% names(events))) {
    stop("'time_var' is not a column name in all time series data frames")
  }

  # Ensure time_var variable in bundle data frames is class POSIXct
  for (i in seq_len(length(bundle_list))) {
    if (!("POSIXct" %in% class(bundle_list[[i]][[time_var]]))) {
      stop(paste0("'time_var' column in all time series data frames",
        " must be POSIXct class"))
    }
  }

  # Ensure time_var variable in events data frame is class POSIXct
  if (!("POSIXct" %in% class(events[[time_var]]))) {
    stop(paste0("'time_var' column in all time series data frames",
      " must be POSIXct class"))
  }

  # Number of bunele names provided matches number of data frames passed
  if (length(bundle_list) != length(bundle_names)) {
    stop(paste0("Need to pass a name for each bundle data frame. The number",
      " of data frames does not equal the number of names."))
  }

  ########### Prep data for joins ---------------------------------------------
  ## Bundle data frames
  for (i in seq_len(length(bundle_list))) {
    # Subset data frames
    bundle_list[[i]] <- data.table(
      bundle_list[[i]][, c(join_key, time_var), with = FALSE]
    )
    
    # Create bundle variable
    bundle_list[[i]][, c(bundle_names[i]) := get(time_var)]
    
    # Set keys
    setkeyv(bundle_list[[i]], c(join_key, time_var, bundle_names[i]))
  }

  ## event data frame
  # Subset data frame
  events <- data.table(events[, c(join_key, time_var), with = FALSE])

  # Change name
  setnames(events, time_var, event_name)

  ########### Overlap joins ---------------------------------------------------
  # Initialize bundle start and end variables
  BUNDLE_START = NULL
  BUNDLE_END = NULL

  # Go through different cases of time window arguments
  if (length(window_hours_pre) == 1 & length(window_hours_post) == 1) {
    # Build bundle window
    events[, BUNDLE_START := get(event_name) - (window_hours_pre * 60 * 60)]
    events[, BUNDLE_END := get(event_name) + (window_hours_post * 60 * 60)]

    ## Set keys
    setkeyv(events, c(join_key, "BUNDLE_START", "BUNDLE_END"))

    ## Loop through every bundle item
    for (i in seq_len(length(bundle_list))) {
    
      # Add column for each bundle item to event table
      events <- foverlaps(events, bundle_list[[i]], by.x = c(join_key,
                          "BUNDLE_START", "BUNDLE_END"), by.y = c(join_key,
                          time_var, bundle_names[i]), mult = mult)
      
      ## Drop extra column
      events[,c(time_var) := NULL]
    }
  }

  if (length(window_hours_pre) == 1 & length(window_hours_post) != 1) {
    # Build bundle start time for all bundle items
    events[, BUNDLE_START := get(event_name) - (window_hours_pre * 60 * 60)]

    ## Loop through every bundle item
    for (i in seq_len(length(bundle_list))) {
      # Add bundle end based on hour for specific item
      events[, BUNDLE_END := get(event_name) + 
        (window_hours_post[i] * 60 * 60)]

      # Set keys
      setkeyv(events, c(join_key, "BUNDLE_START", "BUNDLE_END"))
    
      # Add column for each bundle item to event table
      events <- foverlaps(events, bundle_list[[i]], by.x = c(join_key,
                          "BUNDLE_START", "BUNDLE_END"), by.y = c(join_key,
                          time_var, bundle_names[i]), mult = mult)
      
      ## Drop extra column
      events[,c(time_var) := NULL]
    }
  }

  if (length(window_hours_pre) != 1 & length(window_hours_post) == 1) {
    # Build bundle end time for all bundle items
    events[, BUNDLE_END := get(event_name) + (window_hours_post * 60 * 60)]

    ## Loop through every bundle item
    for (i in seq_len(length(bundle_list))) {
      # Add bundle start based on hour for specific item
      events[, BUNDLE_START := get(event_name) - 
        (window_hours_pre[i] * 60 * 60)]

      # Set keys
      setkeyv(events, c(join_key, "BUNDLE_START", "BUNDLE_END"))
    
      # Add column for each bundle item to event table
      events <- foverlaps(events, bundle_list[[i]], by.x = c(join_key,
                          "BUNDLE_START", "BUNDLE_END"), by.y = c(join_key,
                          time_var, bundle_names[i]), mult = mult)
      
      ## Drop extra column
      events[,c(time_var) := NULL]
    }
  }

  if (length(window_hours_pre) != 1 & length(window_hours_post) != 1) {
    ## Loop through every bundle item
    for (i in seq_len(length(bundle_list))) {
      # Add bundle start and end times based on hour for specific item
      events[, BUNDLE_START := get(event_name) - 
        (window_hours_pre[i] * 60 * 60)]
      events[, BUNDLE_END := get(event_name) + 
        (window_hours_post[i] * 60 * 60)]

      # Set keys
      setkeyv(events, c(join_key, "BUNDLE_START", "BUNDLE_END"))
    
      # Add column for each bundle item to event table
      events <- foverlaps(events, bundle_list[[i]], by.x = c(join_key,
                          "BUNDLE_START", "BUNDLE_END"), by.y = c(join_key,
                          time_var, bundle_names[i]), mult = mult)
      
      ## Drop extra column
      events[,c(time_var) := NULL]
    }
  }
  
  ########### Clean before writing out ---------------------------------------
  # Drop bundle start and end times
  events[, c("BUNDLE_START", "BUNDLE_END") := NULL]

  # Fix column order
  setcolorder(events, c(join_key, event_name, bundle_names))

  # Order by join key and time variable
  setkeyv(events, c(join_key, event_name))
  
  ############ Return event time table ----------------------------------------
  return(events)
}