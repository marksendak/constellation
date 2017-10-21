library(constellation)
context("Constellate")

## Build test patient
crea_testpt <- labs[VARIABLE == "CREATININE" & PAT_ID == "108546"]
plts_testpt <- labs[VARIABLE == "PLATELETS" & PAT_ID == "108546"]

## Tests
test_that("constellate produces expected values for test patient", {
  ####### all events
  crea_plts <- rbind(
      data.table(PAT_ID = 108546, TEST_TIME =
                     fastPOSIXct("2010-02-28 22:49:15", tz = "GMT")),
      data.table(PAT_ID = 108546, TEST_TIME =
                     fastPOSIXct("2010-03-01 08:57:15", tz = "GMT")),
      data.table(PAT_ID = 108546, TEST_TIME =
                     fastPOSIXct("2010-03-02 08:50:45", tz = "GMT"))
  )
  setkeyv(crea_plts, c("PAT_ID", "TEST_TIME"))

  ## Test
  expect_equal(head(constellate(crea_testpt, plts_testpt, window_hours = 2,
    join_key = "PAT_ID", time_var = "RECORDED_TIME", event_name = "TEST",
    mult = "all"), n = 3), crea_plts)
  
  ####### first event per patient
  crea_plts <- data.table(PAT_ID = 108546,
    TEST_TIME = fastPOSIXct("2010-02-28 22:49:15", tz = "GMT"))
  setkey(crea_plts, "PAT_ID")
  
  ## Test
  expect_equal(constellate(crea_testpt, plts_testpt, window_hours = 2,
    join_key = "PAT_ID", time_var = "RECORDED_TIME", event_name = "TEST",
    mult = "first"), crea_plts)
  
  ####### last event per patient
  crea_plts <- data.table(PAT_ID = 108546,
    TEST_TIME = fastPOSIXct("2010-06-29 15:28:15", tz = "GMT"))
  setkey(crea_plts, "PAT_ID")
  
  ## Test
  expect_equal(constellate(crea_testpt, plts_testpt, window_hours = 2,
    join_key = "PAT_ID", time_var = "RECORDED_TIME", event_name = "TEST",
    mult = "last"), crea_plts)
})

test_that("event name assigns properly", {
  crea_plts <- rbind(
    data.table(PAT_ID = 108546, BLAH_TIME =
                 fastPOSIXct("2010-02-28 22:49:15", tz = "GMT")),
    data.table(PAT_ID = 108546, BLAH_TIME =
                 fastPOSIXct("2010-03-01 08:57:15", tz = "GMT")),
    data.table(PAT_ID = 108546, BLAH_TIME =
                 fastPOSIXct("2010-03-02 08:50:45", tz = "GMT"))
  )
  setkeyv(crea_plts, c("PAT_ID", "BLAH_TIME"))
  
  ## Test
  expect_equal(head(constellate(crea_testpt, plts_testpt, window_hours = 2,
    join_key = "PAT_ID", time_var = "RECORDED_TIME", event_name = "BLAH",
    mult = "all"), n = 3), crea_plts)
})

test_that("window hours roll over", {
  expect_equal(
    constellate(crea_testpt, plts_testpt, window_hours = 2,
      join_key = "PAT_ID", time_var = "RECORDED_TIME", event_name = "TEST",
      mult = "all"), 
    constellate(crea_testpt, plts_testpt, window_hours = c(2,2),
      join_key = "PAT_ID", time_var = "RECORDED_TIME", event_name = "TEST",
      mult = "all")
  )
  
  expect_equal(
    constellate(crea_testpt, plts_testpt, window_hours = 4,
      join_key = "PAT_ID", time_var = "RECORDED_TIME", event_name = "TEST",
                mult = "all"), 
    constellate(crea_testpt, plts_testpt, window_hours = c(4,4),
      join_key = "PAT_ID", time_var = "RECORDED_TIME", event_name = "TEST",
      mult = "all")
  )
  
  ## Remove objects
  rm(crea_plts)
})

test_that("default arguments function properly", {
  ## Function output is identical when you drop mult argument and set to all
  expect_equal(
    constellate(crea_testpt, plts_testpt, window_hours = 2,
      join_key = "PAT_ID", time_var = "RECORDED_TIME", event_name = "TEST"),
    constellate(crea_testpt, plts_testpt, window_hours = 2,
      join_key = "PAT_ID", time_var = "RECORDED_TIME", event_name = "TEST",
      mult = "all")
  )
})

test_that("error messages function", {
  ## Missing arguments
  expect_error(
    constellate(crea_testpt, window_hours = 2, join_key = "PAT_ID",
      time_var = "RECORDED_TIME", event_name = "TEST", mult = "all"),
    "Need to pass at least two time series data frames"
  )
  expect_error(
    constellate(window_hours = 2, join_key = "PAT_ID",
      time_var = "RECORDED_TIME", event_name = "TEST", mult = "all"),
    "Need to pass at least two time series data frames"
  )
  expect_error(
    constellate(crea_testpt, plts_testpt, join_key = "PAT_ID",
      time_var = "RECORDED_TIME", event_name = "TEST", mult = "all"),
    "Need to specify window hours"
  )
  expect_error(
    constellate(crea_testpt, plts_testpt, window_hours = 2,
      time_var = "RECORDED_TIME", event_name = "TEST", mult = "all"),
    "Need to specify join key"
  )
  expect_error(
    constellate(crea_testpt, plts_testpt, window_hours = 2,
      join_key = "PAT_ID", event_name = "TEST", mult = "all"),
    "Need to specify time variable"
  )
  expect_error(
    constellate(crea_testpt, plts_testpt, window_hours = 2, 
      join_key = "PAT_ID", time_var = "RECORDED_TIME", mult = "all"),
    "Need to specify event name"
  )
  
  ## Arguments don't match
  expect_error(
    constellate(crea_testpt, plts_testpt, window_hours = 2,
      join_key = "PAT_ID", time_var = "RECORDED_TIME", event_name = "TEST",
      mult = "foo"),
    "'arg' should be one of"
  )
  
  ## Appropriate classes and values
  expect_error(
    constellate("foo", plts_testpt, window_hours = 2, join_key = "PAT_ID",
      time_var = "RECORDED_TIME", event_name = "TEST", mult = "all"),
    "Need to pass only data frames in first argument"
  )
  expect_error(
    constellate(crea_testpt, plts_testpt, window_hours = 2,
      join_key = "PAT_ID", time_var = "RECORDED_TIME", event_name = 1,
      mult = "all"),
    "'event_name' must be a character string"
  )
  expect_error(
    constellate(crea_testpt, plts_testpt, window_hours = "2",
      join_key = "PAT_ID", time_var = "RECORDED_TIME", event_name = "TEST",
      mult = "all"),
    "All window_hours must be numeric"
  )
  expect_error(
    constellate(crea_testpt, plts_testpt, window_hours = -5,
      join_key = "PAT_ID", time_var = "RECORDED_TIME", event_name = "TEST",
      mult = "all"),
    "All window_hours must be greater than 0"
  )
  
  ## Missing column
  expect_error(
    constellate(crea_testpt, plts_testpt, window_hours = 2,
      join_key = "foo", time_var = "RECORDED_TIME", event_name = "TEST",
      mult = "all"),
    "'join_key' is not a column name in all time series data frames" 
  )
  expect_error(
    constellate(crea_testpt, plts_testpt, window_hours = 2,
      join_key = "PAT_ID", time_var = "foo", event_name = "TEST",
      mult = "all"),
    "'time_var' is not a column name in all time series data frames" 
  )
  
  ## Same number of window hours as data frames
  expect_error(
    constellate(crea_testpt, plts_testpt, window_hours = c(2, 2, 2),
      join_key = "PAT_ID", time_var = "RECORDED_TIME", event_name = "TEST",
      mult = "all"),
    paste0("Need to pass a single window hour length for all criteria data",
      " frames or a window hour length for each criteria data frame.")
  )
})