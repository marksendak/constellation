library(constellation)
context("Incidents")

## Set timezone
Sys.setenv(TZ = "UTC")

## Build test patient
pulse_testpts <- vitals[VARIABLE == "PULSE" & PAT_ID %in% c("108546", "450516")]

## Set time variables to POSIXct
pulse_testpts <- pulse_testpts[, RECORDED_TIME := as.POSIXct(RECORDED_TIME,
  format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")]

## Round value
pulse_testpts[, VALUE := round(VALUE, digits = 5)]

## Set keys
setkeyv(pulse_testpts, c("PAT_ID", "RECORDED_TIME"))

## Tests
test_that("incidents produces expected values for test patient", {
  ####### Grouping observations by patient
  pulse_incidents <- rbind(
    data.table(PAT_ID = 108546,
      RECORDED_TIME = as.POSIXct("2010-02-25 05:46:27", tz = "UTC"),
      VALUE = 114.48809, VARIABLE = "PULSE"),
    data.table(PAT_ID = 108546,
      RECORDED_TIME = as.POSIXct("2010-02-26 06:11:06", tz = "UTC"),
      VALUE = 107.37308, VARIABLE = "PULSE"),
    data.table(PAT_ID = 108546,
      RECORDED_TIME = as.POSIXct("2010-02-27 07:00:22", tz = "UTC"),
      VALUE = 94.55696, VARIABLE = "PULSE"),
    data.table(PAT_ID = 450516,
      RECORDED_TIME = as.POSIXct("2010-08-23 05:04:36", tz = "UTC"),
      VALUE = 105.70947, VARIABLE = "PULSE"),
    data.table(PAT_ID = 450516,
      RECORDED_TIME = as.POSIXct("2010-08-24 07:43:11", tz = "UTC"),
      VALUE = 92.29679, VARIABLE = "PULSE"),
    data.table(PAT_ID = 450516,
      RECORDED_TIME = as.POSIXct("2010-08-25 08:30:19", tz = "UTC"),
      VALUE = 70.87019, VARIABLE = "PULSE"))
  setkeyv(pulse_incidents, c("PAT_ID"))
  
  ## Test
  expect_equal(incidents(pulse_testpts, window_hours = 24,
    time_var = "RECORDED_TIME", join_key = "PAT_ID")[,.SD[1:3], by = PAT_ID],
    pulse_incidents)
  
  ####### No grouping observations by patient
  pulse_incidents <- rbind(
    data.table(PAT_ID = 108546,
               RECORDED_TIME = as.POSIXct("2010-02-25 05:46:27", tz = "UTC"),
               VALUE = 114.48809, VARIABLE = "PULSE"),
    data.table(PAT_ID = 108546,
               RECORDED_TIME = as.POSIXct("2010-02-26 06:11:06", tz = "UTC"),
               VALUE = 107.37308, VARIABLE = "PULSE"),
    data.table(PAT_ID = 108546,
               RECORDED_TIME = as.POSIXct("2010-02-27 07:00:22", tz = "UTC"),
               VALUE = 94.55696, VARIABLE = "PULSE"))
  setkeyv(pulse_incidents, c("RECORDED_TIME"))
  
  ## Test
  expect_equal(incidents(pulse_testpts, window_hours = 24,
    time_var = "RECORDED_TIME")[1:3], pulse_incidents)

  ## Remove objects
  rm(pulse_incidents)
})

test_that("error messages function", {
  ## Missing arguments
  expect_error(
    incidents(window_hours = 24, time_var = "RECORDED_TIME"),
    "Need to pass data frame"
  )
  expect_error(
    incidents(pulse_testpts, time_var = "RECORDED_TIME"),
    "Need to specify window hours"
  )
  expect_error(
    incidents(pulse_testpts, window_hours = 24),
    "Need to specify time variable"
  )
  
  ## Appropriate classes
  expect_error(
    incidents("foo", window_hours = 24, time_var = "RECORDED_TIME"),
    "'data' must be a data.frame"
  )
  expect_error(
    incidents(pulse_testpts, window_hours = "24", time_var = "RECORDED_TIME"),
    "'window_hours' must be numeric"
  )
  
  ## Missing column from bundle data frames
  expect_error(
    incidents(pulse_testpts, window_hours = 24, time_var = "foo"),
    "'time_var' is not a column name in data"
  )
  expect_error(
    incidents(pulse_testpts, window_hours = 24, time_var = "RECORDED_TIME",
      join_key = "foo"),
    "'join_key' is not a column name in data"
  )
  
  ## Time variable is not POSIXct
  pulse_testpts[, RECORDED_TIME := as.Date(RECORDED_TIME)]
  expect_error(
    incidents(pulse_testpts, window_hours = 24, time_var = "RECORDED_TIME"),
    "'time_var' column in data must be POSIXct class"
  )
})
