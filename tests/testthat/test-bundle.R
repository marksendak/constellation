library(constellation)
context("Bundle")

## Build test patient
crea_testpt <- labs[VARIABLE == "CREATININE" & PAT_ID == "108546"]
plts_testpt <- labs[VARIABLE == "PLATELETS" & PAT_ID == "108546"]
inr_testpt <- labs[VARIABLE == "INR" & PAT_ID == "108546"]

## Tests
test_that("bundle produces expected values for test patient", {
  ####### all events
  crea_bundle <- rbind(
      data.table(PAT_ID = 108546, CREATININE =
                     fastPOSIXct("2010-02-26 01:48:18", tz = "GMT"),
                     PLATELETS =
                     fastPOSIXct("2010-02-25 10:27:44", tz = "GMT"),
                     INR = fastPOSIXct("2010-02-26 05:15:30", tz = "GMT")),
      data.table(PAT_ID = 108546, CREATININE =
                     fastPOSIXct("2010-02-28 21:41:50", tz = "GMT"),
                     PLATELETS =
                     fastPOSIXct("2010-02-28 09:27:15", tz = "GMT"),
                     INR = fastPOSIXct(NA, tz = "GMT")),
      data.table(PAT_ID = 108546, CREATININE =
                     fastPOSIXct("2010-02-28 21:41:50", tz = "GMT"),
                     PLATELETS =
                     fastPOSIXct("2010-02-28 22:49:15", tz = "GMT"),
                     INR = fastPOSIXct(NA, tz = "GMT"))
  )
  setkeyv(crea_bundle, c("PAT_ID", "CREATININE"))

  ## Test
  expect_equal(head(bundle(crea_testpt, plts_testpt, inr_testpt,
    bundle_names = c("PLATELETS", "INR"), window_hours_pre = c(24, 24),
    window_hours_post = c(6, 6), join_key = "PAT_ID",
    time_var = "RECORDED_TIME", event_name = "CREATININE", mult = "all"),
    n = 3), crea_bundle)
  
  ####### first event per patient
  crea_bundle <- rbind(
      data.table(PAT_ID = 108546, CREATININE =
                     fastPOSIXct("2010-02-26 01:48:18", tz = "GMT"),
                     PLATELETS =
                     fastPOSIXct("2010-02-25 10:27:44", tz = "GMT"),
                     INR = fastPOSIXct("2010-02-26 05:15:30", tz = "GMT")),
      data.table(PAT_ID = 108546, CREATININE =
                     fastPOSIXct("2010-02-28 21:41:50", tz = "GMT"),
                     PLATELETS =
                     fastPOSIXct("2010-02-28 09:27:15", tz = "GMT"),
                     INR = fastPOSIXct(NA, tz = "GMT")),
      data.table(PAT_ID = 108546, CREATININE =
                     fastPOSIXct("2010-03-01 08:57:15", tz = "GMT"),
                     PLATELETS =
                     fastPOSIXct("2010-02-28 09:27:15", tz = "GMT"),
                     INR = fastPOSIXct("2010-03-01 09:12:55", tz = "GMT"))
  )
  setkeyv(crea_bundle, c("PAT_ID", "CREATININE"))
  
  ## Test
  expect_equal(head(bundle(crea_testpt, plts_testpt, inr_testpt,
    bundle_names = c("PLATELETS", "INR"), window_hours_pre = c(24, 24),
    window_hours_post = c(6, 6), join_key = "PAT_ID",
    time_var = "RECORDED_TIME", event_name = "CREATININE", mult = "first"),
    n = 3), crea_bundle)
  
  ####### last event per patient
  crea_bundle <- rbind(
      data.table(PAT_ID = 108546, CREATININE =
                     fastPOSIXct("2010-02-26 01:48:18", tz = "GMT"),
                     PLATELETS =
                     fastPOSIXct("2010-02-25 10:27:44", tz = "GMT"),
                     INR = fastPOSIXct("2010-02-26 05:15:30", tz = "GMT")),
      data.table(PAT_ID = 108546, CREATININE =
                     fastPOSIXct("2010-02-28 21:41:50", tz = "GMT"),
                     PLATELETS =
                     fastPOSIXct("2010-02-28 22:49:15", tz = "GMT"),
                     INR = fastPOSIXct(NA, tz = "GMT")),
      data.table(PAT_ID = 108546, CREATININE =
                     fastPOSIXct("2010-03-01 08:57:15", tz = "GMT"),
                     PLATELETS =
                     fastPOSIXct("2010-03-01 11:14:15", tz = "GMT"),
                     INR = fastPOSIXct("2010-03-01 09:12:55", tz = "GMT"))
  )
  setkeyv(crea_bundle, c("PAT_ID", "CREATININE"))
  
  ## Test
  expect_equal(head(bundle(crea_testpt, plts_testpt, inr_testpt,
    bundle_names = c("PLATELETS", "INR"), window_hours_pre = c(24, 24),
    window_hours_post = c(6, 6), join_key = "PAT_ID",
    time_var = "RECORDED_TIME", event_name = "CREATININE", mult = "last"),
    n = 3), crea_bundle)

  ## Remove objects
  rm(crea_bundle)
})

test_that("bundle names assign properly", {
  ####### test bundle names for all events
  crea_bundle <- rbind(
      data.table(PAT_ID = 108546, CREATININE =
                     fastPOSIXct("2010-02-26 01:48:18", tz = "GMT"),
                     BUNDLE_1 =
                     fastPOSIXct("2010-02-25 10:27:44", tz = "GMT"),
                     BUNDLE_2 = 
                     fastPOSIXct("2010-02-26 05:15:30", tz = "GMT")),
      data.table(PAT_ID = 108546, CREATININE =
                     fastPOSIXct("2010-02-28 21:41:50", tz = "GMT"),
                     BUNDLE_1 =
                     fastPOSIXct("2010-02-28 09:27:15", tz = "GMT"),
                     BUNDLE_2 = fastPOSIXct(NA, tz = "GMT")),
      data.table(PAT_ID = 108546, CREATININE =
                     fastPOSIXct("2010-02-28 21:41:50", tz = "GMT"),
                     BUNDLE_1 =
                     fastPOSIXct("2010-02-28 22:49:15", tz = "GMT"),
                     BUNDLE_2 = fastPOSIXct(NA, tz = "GMT"))
  )
  setkeyv(crea_bundle, c("PAT_ID", "CREATININE"))

  ## Test
  expect_equal(head(bundle(crea_testpt, plts_testpt, inr_testpt,
    bundle_names = c("BUNDLE_1", "BUNDLE_2"), window_hours_pre = c(24, 24),
    window_hours_post = c(6, 6), join_key = "PAT_ID",
    time_var = "RECORDED_TIME", event_name = "CREATININE", mult = "all"),
    n = 3), crea_bundle)

  ## Remove objects
  rm(crea_bundle)
})

test_that("event name assigns properly", {
  ####### all events
  crea_bundle <- rbind(
      data.table(PAT_ID = 108546, BLAH =
                     fastPOSIXct("2010-02-26 01:48:18", tz = "GMT"),
                     PLATELETS =
                     fastPOSIXct("2010-02-25 10:27:44", tz = "GMT"),
                     INR = fastPOSIXct("2010-02-26 05:15:30", tz = "GMT")),
      data.table(PAT_ID = 108546, BLAH =
                     fastPOSIXct("2010-02-28 21:41:50", tz = "GMT"),
                     PLATELETS =
                     fastPOSIXct("2010-02-28 09:27:15", tz = "GMT"),
                     INR = fastPOSIXct(NA, tz = "GMT")),
      data.table(PAT_ID = 108546, BLAH = 
                     fastPOSIXct("2010-02-28 21:41:50", tz = "GMT"),
                     PLATELETS =
                     fastPOSIXct("2010-02-28 22:49:15", tz = "GMT"),
                     INR = fastPOSIXct(NA, tz = "GMT"))
  )
  setkeyv(crea_bundle, c("PAT_ID", "BLAH"))
  
  ## Test
  expect_equal(head(bundle(crea_testpt, plts_testpt, inr_testpt,
    bundle_names = c("PLATELETS", "INR"), window_hours_pre = c(24, 24),
    window_hours_post = c(6, 6), join_key = "PAT_ID",
    time_var = "RECORDED_TIME", event_name = "BLAH", mult = "all"),
    n = 3), crea_bundle)

  ## Remove objects
  rm(crea_bundle)
})

test_that("window hours roll over", {
  expect_equal(
    bundle(crea_testpt, plts_testpt, inr_testpt,
      bundle_names = c("PLATELETS", "INR"), window_hours_pre = 24,
      window_hours_post = c(6, 6), join_key = "PAT_ID",
      time_var = "RECORDED_TIME", event_name = "CREATININE", mult = "all"),
    bundle(crea_testpt, plts_testpt, inr_testpt,
      bundle_names = c("PLATELETS", "INR"), window_hours_pre = c(24, 24),
      window_hours_post = c(6, 6), join_key = "PAT_ID",
      time_var = "RECORDED_TIME", event_name = "CREATININE", mult = "all")
  )

  expect_equal(
    bundle(crea_testpt, plts_testpt, inr_testpt,
      bundle_names = c("PLATELETS", "INR"), window_hours_pre = c(24, 24),
      window_hours_post = 6, join_key = "PAT_ID",
      time_var = "RECORDED_TIME", event_name = "CREATININE", mult = "all"),
    bundle(crea_testpt, plts_testpt, inr_testpt,
      bundle_names = c("PLATELETS", "INR"), window_hours_pre = c(24, 24),
      window_hours_post = c(6, 6), join_key = "PAT_ID",
      time_var = "RECORDED_TIME", event_name = "CREATININE", mult = "all")
  )

  expect_equal(
    bundle(crea_testpt, plts_testpt, inr_testpt,
      bundle_names = c("PLATELETS", "INR"), window_hours_pre = 24,
      window_hours_post = 6, join_key = "PAT_ID",
      time_var = "RECORDED_TIME", event_name = "CREATININE", mult = "all"),
    bundle(crea_testpt, plts_testpt, inr_testpt,
      bundle_names = c("PLATELETS", "INR"), window_hours_pre = c(24, 24),
      window_hours_post = c(6, 6), join_key = "PAT_ID",
      time_var = "RECORDED_TIME", event_name = "CREATININE", mult = "all")
  )
})

test_that("default arguments function properly", {
  ## Function output is identical when you drop mult argument and set to all
  expect_equal(
    bundle(crea_testpt, plts_testpt, inr_testpt,
      bundle_names = c("PLATELETS", "INR"), window_hours_pre = c(24, 24),
      window_hours_post = c(6, 6), join_key = "PAT_ID",
      time_var = "RECORDED_TIME", event_name = "CREATININE"),
    bundle(crea_testpt, plts_testpt, inr_testpt,
      bundle_names = c("PLATELETS", "INR"), window_hours_pre = c(24, 24),
      window_hours_post = c(6, 6), join_key = "PAT_ID",
      time_var = "RECORDED_TIME", event_name = "CREATININE", mult = "all")
  )
})

test_that("error messages function", {
  ## Missing arguments
  expect_error(
    bundle(crea_testpt,
      bundle_names = c("PLATELETS", "INR"), window_hours_pre = c(24, 24),
      window_hours_post = c(6, 6), join_key = "PAT_ID",
      time_var = "RECORDED_TIME", event_name = "CREATININE", mult = "all"),
    "Need to pass at least one bundle item data frame"
  )
  expect_error(
    bundle(crea_testpt, plts_testpt, inr_testpt,
      window_hours_pre = c(24, 24),
      window_hours_post = c(6, 6), join_key = "PAT_ID",
      time_var = "RECORDED_TIME", event_name = "CREATININE", mult = "all"),
    "Need to provide bundle names"
  )
  expect_error(
    bundle(crea_testpt, plts_testpt, inr_testpt,
      bundle_names = c("PLATELETS", "INR"),
      window_hours_post = c(6, 6), join_key = "PAT_ID",
      time_var = "RECORDED_TIME", event_name = "CREATININE", mult = "all"),
    "Need to specify window hours before the event"
  )
  expect_error(
    bundle(crea_testpt, plts_testpt, inr_testpt,
      bundle_names = c("PLATELETS", "INR"), window_hours_pre = c(24, 24),
      join_key = "PAT_ID",
      time_var = "RECORDED_TIME", event_name = "CREATININE", mult = "all"),
    "Need to specify window hours after the event"
  )
  expect_error(
    bundle(crea_testpt, plts_testpt, inr_testpt,
      bundle_names = c("PLATELETS", "INR"), window_hours_pre = c(24, 24),
      window_hours_post = c(6, 6),
      time_var = "RECORDED_TIME", event_name = "CREATININE", mult = "all"),
    "Need to specify join key"
  )
  expect_error(
    bundle(crea_testpt, plts_testpt, inr_testpt,
      bundle_names = c("PLATELETS", "INR"), window_hours_pre = c(24, 24),
      window_hours_post = c(6, 6), join_key = "PAT_ID",
      event_name = "CREATININE", mult = "all"),
    "Need to specify time variable"
  )
  expect_error(
    bundle(crea_testpt, plts_testpt, inr_testpt,
      bundle_names = c("PLATELETS", "INR"), window_hours_pre = c(24, 24),
      window_hours_post = c(6, 6), join_key = "PAT_ID",
      time_var = "RECORDED_TIME", mult = "all"),
    "Need to specify an event name"
  )

  ## Arguments don't match
  expect_error(
    bundle(crea_testpt, plts_testpt, inr_testpt,
      bundle_names = c("PLATELETS", "INR"), window_hours_pre = c(24, 24),
      window_hours_post = c(6, 6), join_key = "PAT_ID",
      time_var = "RECORDED_TIME", event_name = "CREATININE", mult = "foo"),
    "'arg' should be one of"
  )

  ## Appropriate classes and values
  expect_error(
    bundle("foo", plts_testpt, inr_testpt,
      bundle_names = c("PLATELETS", "INR"), window_hours_pre = c(24, 24),
      window_hours_post = c(6, 6), join_key = "PAT_ID",
      time_var = "RECORDED_TIME", event_name = "CREATININE", mult = "all"),
    "Need to pass data frame in first argument"
  )
  expect_error(
    bundle(crea_testpt, "foo", "foo2",
      bundle_names = c("PLATELETS", "INR"), window_hours_pre = c(24, 24),
      window_hours_post = c(6, 6), join_key = "PAT_ID",
      time_var = "RECORDED_TIME", event_name = "CREATININE", mult = "all"),
    "Need to pass only data frames as bundle items"
  )
  expect_error(
    bundle(crea_testpt, plts_testpt, inr_testpt,
      bundle_names = c(2, 2), window_hours_pre = c(24, 24),
      window_hours_post = c(6, 6), join_key = "PAT_ID",
      time_var = "RECORDED_TIME", event_name = "CREATININE", mult = "all"),
    "All bundle_names must be strings"
  )
  expect_error(
    bundle(crea_testpt, plts_testpt, inr_testpt,
      bundle_names = c("PLATELETS", "INR"), window_hours_pre = c("24", "24"),
      window_hours_post = c(6, 6), join_key = "PAT_ID",
      time_var = "RECORDED_TIME", event_name = "CREATININE", mult = "all"),
    "All window_hours_pre must be numeric"
  )
  expect_error(
    bundle(crea_testpt, plts_testpt, inr_testpt,
      bundle_names = c("PLATELETS", "INR"), window_hours_pre = c(24, 24),
      window_hours_post = c("6", "6"), join_key = "PAT_ID",
      time_var = "RECORDED_TIME", event_name = "CREATININE", mult = "all"),
    "All window_hours_post must be numeric"
  )
  expect_error(
    bundle(crea_testpt, plts_testpt, inr_testpt,
      bundle_names = c("PLATELETS", "INR"), window_hours_pre = c(24, 24),
      window_hours_post = c(6, 6), join_key = "PAT_ID",
      time_var = "RECORDED_TIME", event_name = 2, mult = "all"),
    "'event_name' must be a character string"
  )

  ## Missing column
  expect_error(
    bundle(crea_testpt, plts_testpt, inr_testpt,
      bundle_names = c("PLATELETS", "INR"), window_hours_pre = c(24, 24),
      window_hours_post = c(6, 6), join_key = "foo",
      time_var = "RECORDED_TIME", event_name = "CREATININE", mult = "all"),
    "'join_key' is not a column name in all time series data frames" 
  )
  expect_error(
    bundle(crea_testpt, plts_testpt, inr_testpt,
      bundle_names = c("PLATELETS", "INR"), window_hours_pre = c(24, 24),
      window_hours_post = c(6, 6), join_key = "PAT_ID",
      time_var = "foo", event_name = "CREATININE", mult = "all"),
    "'time_var' is not a column name in all time series data frames" 
  )
  
    bundle(crea_testpt, plts_testpt, inr_testpt,
      bundle_names = c("PLATELETS", "INR"), window_hours_pre = c(24, 24),
      window_hours_post = c(6, 6), join_key = "PAT_ID",
      time_var = "RECORDED_TIME", event_name = "CREATININE", mult = "all")

  ## Same number of window hours as bundle data frames
  expect_error(
    bundle(crea_testpt, plts_testpt, inr_testpt,
      bundle_names = c("PLATELETS", "INR"), window_hours_pre = c(24, 24, 24),
      window_hours_post = c(6, 6), join_key = "PAT_ID",
      time_var = "RECORDED_TIME", event_name = "CREATININE", mult = "all"),
    paste0("Need to pass a single 'window_hours_pre' value for all",
      " bundle data frames or a 'window_hours_pre' value for each bundle data",
      " frame.")
  )
  expect_error(
    bundle(crea_testpt, plts_testpt, inr_testpt,
      bundle_names = c("PLATELETS", "INR"), window_hours_pre = c(24, 24),
      window_hours_post = c(6, 6, 6), join_key = "PAT_ID",
      time_var = "RECORDED_TIME", event_name = "CREATININE", mult = "all"),
    paste0("Need to pass a single 'window_hours_post' value for all",
      " bundle data frames or a 'window_hours_post' value for each bundle",
      " data frame.")
  )
})