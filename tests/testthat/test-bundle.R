library(constellation)
context("Bundle")

## Set timezone
Sys.setenv(TZ = "UTC")

## Build test patient
crea_testpt <- labs[VARIABLE == "CREATININE" & PAT_ID == "108546"]
plts_testpt <- labs[VARIABLE == "PLATELETS" & PAT_ID == "108546"]
inr_testpt <- labs[VARIABLE == "INR" & PAT_ID == "108546"]

## Set time variables to POSIXct
crea_testpt <- crea_testpt[, RECORDED_TIME := as.POSIXct(RECORDED_TIME,
  format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")]
plts_testpt <- plts_testpt[, RECORDED_TIME := as.POSIXct(RECORDED_TIME,
  format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")]
inr_testpt <- inr_testpt[, RECORDED_TIME := as.POSIXct(RECORDED_TIME,
  format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")]

## Tests
test_that("bundle produces expected values for test patient", {
  ####### all events
  crea_bundle <- rbind(
      data.table(PAT_ID = 108546, CREATININE =
                     as.POSIXct("2010-02-26 01:48:18", tz = "UTC"),
                     PLATELETS =
                     as.POSIXct("2010-02-25 10:27:44", tz = "UTC"),
                     INR = as.POSIXct("2010-02-26 05:15:30", tz = "UTC")),
      data.table(PAT_ID = 108546, CREATININE =
                     as.POSIXct("2010-02-28 21:41:50", tz = "UTC"),
                     PLATELETS =
                     as.POSIXct("2010-02-28 09:27:15", tz = "UTC"),
                     INR = as.POSIXct(NA, tz = "UTC")),
      data.table(PAT_ID = 108546, CREATININE =
                     as.POSIXct("2010-02-28 21:41:50", tz = "UTC"),
                     PLATELETS =
                     as.POSIXct("2010-02-28 22:49:15", tz = "UTC"),
                     INR = as.POSIXct(NA, tz = "UTC"))
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
                     as.POSIXct("2010-02-26 01:48:18", tz = "UTC"),
                     PLATELETS =
                     as.POSIXct("2010-02-25 10:27:44", tz = "UTC"),
                     INR = as.POSIXct("2010-02-26 05:15:30", tz = "UTC")),
      data.table(PAT_ID = 108546, CREATININE =
                     as.POSIXct("2010-02-28 21:41:50", tz = "UTC"),
                     PLATELETS =
                     as.POSIXct("2010-02-28 09:27:15", tz = "UTC"),
                     INR = as.POSIXct(NA, tz = "UTC")),
      data.table(PAT_ID = 108546, CREATININE =
                     as.POSIXct("2010-03-01 08:57:15", tz = "UTC"),
                     PLATELETS =
                     as.POSIXct("2010-02-28 09:27:15", tz = "UTC"),
                     INR = as.POSIXct("2010-03-01 09:12:55", tz = "UTC"))
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
                     as.POSIXct("2010-02-26 01:48:18", tz = "UTC"),
                     PLATELETS =
                     as.POSIXct("2010-02-25 10:27:44", tz = "UTC"),
                     INR = as.POSIXct("2010-02-26 05:15:30", tz = "UTC")),
      data.table(PAT_ID = 108546, CREATININE =
                     as.POSIXct("2010-02-28 21:41:50", tz = "UTC"),
                     PLATELETS =
                     as.POSIXct("2010-02-28 22:49:15", tz = "UTC"),
                     INR = as.POSIXct(NA, tz = "UTC")),
      data.table(PAT_ID = 108546, CREATININE =
                     as.POSIXct("2010-03-01 08:57:15", tz = "UTC"),
                     PLATELETS =
                     as.POSIXct("2010-03-01 11:14:15", tz = "UTC"),
                     INR = as.POSIXct("2010-03-01 09:12:55", tz = "UTC"))
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
                     as.POSIXct("2010-02-26 01:48:18", tz = "UTC"),
                     BUNDLE_1 =
                     as.POSIXct("2010-02-25 10:27:44", tz = "UTC"),
                     BUNDLE_2 =
                     as.POSIXct("2010-02-26 05:15:30", tz = "UTC")),
      data.table(PAT_ID = 108546, CREATININE =
                     as.POSIXct("2010-02-28 21:41:50", tz = "UTC"),
                     BUNDLE_1 =
                     as.POSIXct("2010-02-28 09:27:15", tz = "UTC"),
                     BUNDLE_2 = as.POSIXct(NA, tz = "UTC")),
      data.table(PAT_ID = 108546, CREATININE =
                     as.POSIXct("2010-02-28 21:41:50", tz = "UTC"),
                     BUNDLE_1 =
                     as.POSIXct("2010-02-28 22:49:15", tz = "UTC"),
                     BUNDLE_2 = as.POSIXct(NA, tz = "UTC"))
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
                     as.POSIXct("2010-02-26 01:48:18", tz = "UTC"),
                     PLATELETS =
                     as.POSIXct("2010-02-25 10:27:44", tz = "UTC"),
                     INR = as.POSIXct("2010-02-26 05:15:30", tz = "UTC")),
      data.table(PAT_ID = 108546, BLAH =
                     as.POSIXct("2010-02-28 21:41:50", tz = "UTC"),
                     PLATELETS =
                     as.POSIXct("2010-02-28 09:27:15", tz = "UTC"),
                     INR = as.POSIXct(NA, tz = "UTC")),
      data.table(PAT_ID = 108546, BLAH =
                     as.POSIXct("2010-02-28 21:41:50", tz = "UTC"),
                     PLATELETS =
                     as.POSIXct("2010-02-28 22:49:15", tz = "UTC"),
                     INR = as.POSIXct(NA, tz = "UTC"))
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

  ## Same number of window hours as bundle data frames
  expect_error(
    bundle(crea_testpt, plts_testpt, inr_testpt,
             bundle_names = c("PLATELETS", "INR"),
             window_hours_pre = c(24, 24, 24), window_hours_post = c(6, 6),
             join_key = "PAT_ID", time_var = "RECORDED_TIME",
             event_name = "CREATININE", mult = "all"),
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

  ## Missing column from bundle data frames
  setnames(plts_testpt, names(plts_testpt)[1:2], c("foo", "RECORDED_TIME"))
  expect_error(
    bundle(crea_testpt, plts_testpt, inr_testpt,
      bundle_names = c("PLATELETS", "INR"), window_hours_pre = c(24, 24),
      window_hours_post = c(6, 6), join_key = "foo",
      time_var = "RECORDED_TIME", event_name = "CREATININE", mult = "all"),
    "'join_key' is not a column name in all time series data frames"
  )
  setnames(plts_testpt, names(plts_testpt)[1:2], c("PAT_ID", "foo"))
  expect_error(
    bundle(crea_testpt, plts_testpt, inr_testpt,
      bundle_names = c("PLATELETS", "INR"), window_hours_pre = c(24, 24),
      window_hours_post = c(6, 6), join_key = "PAT_ID",
      time_var = "foo", event_name = "CREATININE", mult = "all"),
    "'time_var' is not a column name in all time series data frames"
  )
  setnames(plts_testpt, names(plts_testpt)[1:2], c("PAT_ID", "RECORDED_TIME"))

  ## Missing column from events data frame
  setnames(crea_testpt, names(crea_testpt)[1:2], c("foo", "RECORDED_TIME"))
  expect_error(
    bundle(crea_testpt, plts_testpt, inr_testpt,
         bundle_names = c("PLATELETS", "INR"), window_hours_pre = c(24, 24),
         window_hours_post = c(6, 6), join_key = "foo",
         time_var = "RECORDED_TIME", event_name = "CREATININE", mult = "all"),
    "'join_key' is not a column name in all time series data frames"
  )
  setnames(crea_testpt, names(crea_testpt)[1:2], c("PAT_ID", "foo"))
  expect_error(
    bundle(crea_testpt, plts_testpt, inr_testpt,
         bundle_names = c("PLATELETS", "INR"), window_hours_pre = c(24, 24),
         window_hours_post = c(6, 6), join_key = "PAT_ID",
         time_var = "foo", event_name = "CREATININE", mult = "all"),
    "'time_var' is not a column name in all time series data frames"
  )
  setnames(crea_testpt, names(crea_testpt)[1:2], c("PAT_ID", "RECORDED_TIME"))

  ## Time variable in bundle data frames not POSIXct
  plts_testpt[, RECORDED_TIME := as.Date(RECORDED_TIME)]
  expect_error(
    bundle(crea_testpt, plts_testpt, inr_testpt,
         bundle_names = c("PLATELETS", "INR"), window_hours_pre = c(24, 24),
         window_hours_post = c(6, 6), join_key = "PAT_ID",
         time_var = "RECORDED_TIME", event_name = "CREATININE", mult = "all"),
    "'time_var' column in all time series data frames must be POSIXct class"
  )
  plts_testpt <- labs[VARIABLE == "PLATELETS" & PAT_ID == "108546"]
  plts_testpt <- plts_testpt[, RECORDED_TIME :=
                                 as.POSIXct(RECORDED_TIME, tz = "UTC")]

  ## Time variable in events data frame not POSIXct
  crea_testpt[, RECORDED_TIME := as.Date(RECORDED_TIME)]
  expect_error(
    bundle(crea_testpt, plts_testpt, inr_testpt,
         bundle_names = c("PLATELETS", "INR"), window_hours_pre = c(24, 24),
         window_hours_post = c(6, 6), join_key = "PAT_ID",
         time_var = "RECORDED_TIME", event_name = "CREATININE", mult = "all"),
    "'time_var' column in all time series data frames must be POSIXct class"
  )
  crea_testpt <- labs[VARIABLE == "PLATELETS" & PAT_ID == "108546"]
  crea_testpt <- crea_testpt[, RECORDED_TIME :=
                                 as.POSIXct(RECORDED_TIME, tz = "UTC")]

  ## Number of bundle names doesn't match number of bundle data frames
  expect_error(
    bundle(crea_testpt, plts_testpt, inr_testpt,
         bundle_names = c("PLATELETS", "INR", "foo"),
         window_hours_pre = c(24, 24), window_hours_post = c(6, 6),
         join_key = "PAT_ID", time_var = "RECORDED_TIME",
         event_name = "CREATININE", mult = "all"),
    paste0("Need to pass a name for each bundle data frame. The number",
           " of data frames does not equal the number of names.")
  )
})
