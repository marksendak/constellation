#' Identify when a series of treatment events first occur
#'
#' A function that reads in multiple time series data frames and calculates
#'  the first instant that a series of events occur.
#' The user must names for each event, a variable to use to join the tables, 
#'  and the time stamp variable. This function can ingest an arbitrary number
#'  of data frames with longitudinal time series data.
#' @param ... An arbitrary number of time series data frames that each include
#'  the columns 'join_key' and 'time_var'
#' @param bundle_names A vector of strings specifying the name of each
#'  event. The order of strings in the vector should align with the order of
#'  data frames passed in '...'.
#' @param join_key A string name of the column to join all time series data
#'  frames
#' @param time_var A string name of the time stamp column in all time series
#'  data frames

bundle <- function(..., bundle_names, join_key, time_var) {
  ## Build list
  bundle_list <- list(...)
  
  ########### Prep data for joins ---------------------------------------------
  for (i in seq_len(length(bundle_list))) {
    # Subset data frames
    bundle_list[[i]] <- data.table(
      bundle_list[[i]][, c(join_key, time_var), with = FALSE]
    )
    
    # Ensure class of time_var
    set(bundle_list[[i]], j = time_var,
        value = fastPOSIXct(bundle_list[[i]][[time_var]], tz = "GMT"))
    
    # Create criteria variable
    bundle_list[[i]][, c(bundle_names[i]) := get(time_var)]
    
    # Set keys
    setkeyv(bundle_list[[i]], c(join_key, time_var))
  }
  
  ########### Define parameters -----------------------------------------------
  ## build empty data frame to store events
  event_df <- data.table(NULL)
  
  ########### Rolling joins ---------------------------------------------------
  #### Consider each criteria as a final event
  for (i in seq_len(length(bundle_list))) {

    for (j in setdiff(seq_len(length(bundle_list)), i)) {
      event_subset <- bundle_list[[j]][bundle_list[[i]], roll = T]

      # Update table to join
      bundle_list[[i]] <- event_subset
    }

    # Eliminate added columns from raw event data
    bundle_list[[i]] <- bundle_list[[i]][, c(join_key, time_var,
      bundle_names[i]), with = FALSE]

    # Normalize column order
    setcolorder(event_subset,c(join_key, time_var, bundle_names))

    # Append file to event table
    event_df <- rbind(event_df, event_subset)
  }

  ########### Subset rows and columns ---------------------------------------
  # Drop observations with missing criteria
  for (i in bundle_names) {
   event_df <- subset(event_df, !is.na(get(i)))
  }
  
  ########### Clean before writing out ---------------------------------------
  # Order by join key and time variable
  setkeyv(event_df, c(join_key, time_var))
  
  # Select first row per observation
  event_df <- event_df[, .SD[1], by = join_key]
  
  ############ Return event time table ----------------------------------------
  return(event_df)
}