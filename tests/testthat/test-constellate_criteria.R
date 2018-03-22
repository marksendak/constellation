library(constellation)
context("Constellate and Show Criteria")

## Set timezone
Sys.setenv(TZ = "UTC")

## Build test patient
crea_testpt <- labs[VARIABLE == "CREATININE" & PAT_ID == "108546"]
plts_testpt <- labs[VARIABLE == "PLATELETS" & PAT_ID == "108546"]

## Set time variables to POSIXct
crea_testpt <- crea_testpt[, RECORDED_TIME := as.POSIXct(RECORDED_TIME,
  format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")]
plts_testpt <- plts_testpt[, RECORDED_TIME := as.POSIXct(RECORDED_TIME,
  format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")]

## Tests
test_that("constellate criteria produces expected values for test patient", {
  ####### test lab orders with boolean value
  crea_plts <- rbind(
      data.table(PAT_ID = 108546, RECORDED_TIME =
        as.POSIXct("2010-02-25 10:27:44", tz = "UTC"), CREATININE = 0,
        PLATELETS = 1),
      data.table(PAT_ID = 108546, RECORDED_TIME =
        as.POSIXct("2010-02-26 01:48:18", tz = "UTC"), CREATININE = 1,
        PLATELETS = 0),
      data.table(PAT_ID = 108546, RECORDED_TIME =
        as.POSIXct("2010-02-26 14:36:46", tz = "UTC"), CREATININE = 0,
        PLATELETS = 1)
      )
  crea_plts <- setkeyv(crea_plts, c("PAT_ID", "RECORDED_TIME"))

  ## Test
  expect_equal(head(constellate_criteria(crea_testpt, plts_testpt,
    criteria_names = c("CREATININE", "PLATELETS"), window_hours = 2,
    join_key = "PAT_ID", time_var = "RECORDED_TIME", value = "boolean"),
    n = 3), crea_plts, check.attributes = FALSE)

  ####### test lab orders with time value
  crea_plts <- rbind(
      data.table(PAT_ID = 108546, RECORDED_TIME =
        as.POSIXct("2010-02-25 10:27:44", tz = "UTC"),
        CREATININE = as.POSIXct(NA, tz = "UTC"),
        PLATELETS = as.POSIXct("2010-02-25 10:27:44", tz = "UTC")),
      data.table(PAT_ID = 108546, RECORDED_TIME =
        as.POSIXct("2010-02-26 01:48:18", tz = "UTC"),
        CREATININE = as.POSIXct("2010-02-26 01:48:18", tz = "UTC"),
        PLATELETS = as.POSIXct(NA, tz = "UTC")),
      data.table(PAT_ID = 108546, RECORDED_TIME =
        as.POSIXct("2010-02-26 14:36:46", tz = "UTC"),
        CREATININE = as.POSIXct(NA, tz = "UTC"),
        PLATELETS = as.POSIXct("2010-02-26 14:36:46", tz = "UTC"))
      )
  crea_plts <- setkeyv(crea_plts, c("PAT_ID", "RECORDED_TIME"))

  ## Test
  expect_equal(head(constellate_criteria(crea_testpt, plts_testpt,
    criteria_names = c("CREATININE", "PLATELETS"), window_hours = 2,
    join_key = "PAT_ID", time_var = "RECORDED_TIME", value = "time"),
    n = 3), crea_plts, check.attributes = FALSE)

  ## Remove objects
  rm(crea_plts)

  ####### test lab orders with result value
  crea_plts <- rbind(
      data.table(PAT_ID = 108546, RECORDED_TIME =
        as.POSIXct("2010-02-25 10:27:44", tz = "UTC"),
        CREATININE = NA, PLATELETS = 186.91296),
      data.table(PAT_ID = 108546, RECORDED_TIME =
        as.POSIXct("2010-02-26 01:48:18", tz = "UTC"),
        CREATININE = 0.7804720, PLATELETS = NA),
      data.table(PAT_ID = 108546, RECORDED_TIME =
        as.POSIXct("2010-02-26 14:36:46", tz = "UTC"),
        CREATININE = NA, PLATELETS = 181.77154)
      )
  crea_plts <- setkeyv(crea_plts, c("PAT_ID", "RECORDED_TIME"))

  ## Test
  expect_equal(head(constellate_criteria(crea_testpt, plts_testpt,
    criteria_names = c("CREATININE", "PLATELETS"), window_hours = 2,
    join_key = "PAT_ID", time_var = "RECORDED_TIME", value = "result",
    result_var = "VALUE"), n = 3), crea_plts, tolerance = 1e-5,
    check.attributes = FALSE)

  ## Remove objects
  rm(crea_plts)
})

test_that("criteria names assign properly", {
  ####### test lab orders without final event
  crea_plts <- rbind(
      data.table(PAT_ID = 108546, RECORDED_TIME =
        as.POSIXct("2010-02-25 10:27:44", tz = "UTC"), LAB_1 = 0, LAB_2 = 1),
      data.table(PAT_ID = 108546, RECORDED_TIME =
        as.POSIXct("2010-02-26 01:48:18", tz = "UTC"), LAB_1 = 1, LAB_2 = 0),
      data.table(PAT_ID = 108546, RECORDED_TIME =
        as.POSIXct("2010-02-26 14:36:46", tz = "UTC"), LAB_1 = 0, LAB_2 = 1)
  )
  crea_plts <- setkeyv(crea_plts, c("PAT_ID", "RECORDED_TIME"))

  ## Test
  expect_equal(head(constellate_criteria(crea_testpt, plts_testpt,
    criteria_names = c("LAB_1", "LAB_2"), window_hours = 2,
    join_key = "PAT_ID", time_var = "RECORDED_TIME"), n = 3), crea_plts,
    check.attributes = FALSE)

  ## Remove objects
  rm(crea_plts)
})

test_that("rows added correctly", {
  expect_equal(
    nrow(constellate_criteria(crea_testpt, plts_testpt,
      criteria_names = c("CREATININE", "PLATELETS"), window_hours = 2,
      join_key = "PAT_ID", time_var = "RECORDED_TIME")),
    length(unique(append(crea_testpt[, RECORDED_TIME],
      plts_testpt[, RECORDED_TIME])))
  )
})

test_that("window hours values roll over", {
  expect_equal(
    constellate_criteria(crea_testpt, plts_testpt,
      criteria_names = c("CREATININE", "PLATELETS"), window_hours = 2,
      join_key = "PAT_ID", time_var = "RECORDED_TIME"),
    constellate_criteria(crea_testpt, plts_testpt,
      criteria_names = c("CREATININE", "PLATELETS"), window_hours = c(2, 2),
      join_key = "PAT_ID", time_var = "RECORDED_TIME")
  )
  expect_equal(
    constellate_criteria(crea_testpt, plts_testpt,
      criteria_names = c("CREATININE", "PLATELETS"), window_hours = 4,
      join_key = "PAT_ID", time_var = "RECORDED_TIME"),
    constellate_criteria(crea_testpt, plts_testpt,
      criteria_names = c("CREATININE", "PLATELETS"), window_hours = c(4, 4),
      join_key = "PAT_ID", time_var = "RECORDED_TIME")
  )
})

test_that("default arguments function properly", {
  ## Function output identical when you drop value argument and set to boolean
  expect_equal(
    constellate_criteria(crea_testpt, plts_testpt,
      criteria_names = c("CREATININE", "PLATELETS"), window_hours = 2,
      join_key = "PAT_ID", time_var = "RECORDED_TIME", value = "boolean"),
    constellate_criteria(crea_testpt, plts_testpt,
      criteria_names = c("CREATININE", "PLATELETS"), window_hours = 2,
      join_key = "PAT_ID", time_var = "RECORDED_TIME")
  )
})

test_that("error messages function", {
  ## Missing arguments
  expect_error(
    constellate_criteria(plts_testpt, criteria_names = c("PLATELETS"),
      window_hours = 2, join_key = "PAT_ID", time_var = "RECORDED_TIME"),
    "Need to pass at least two time series data frames"
  )
  expect_error(
    constellate_criteria(criteria_names = c("PLATELETS"), window_hours = 2,
      join_key = "PAT_ID", time_var = "RECORDED_TIME"),
    "Need to pass at least two time series data frames"
  )
  expect_error(
    constellate_criteria(crea_testpt, plts_testpt,
      criteria_names = c("CREATININE", "PLATELETS"), join_key = "PAT_ID",
      time_var = "RECORDED_TIME"),
    "Need to specify window_hours"
  )
  expect_error(
    constellate_criteria(crea_testpt, plts_testpt,
      criteria_names = c("CREATININE", "PLATELETS"), window_hours = 2,
      time_var = "RECORDED_TIME"),
    "Need to specify join_key"
  )
  expect_error(
    constellate_criteria(crea_testpt, plts_testpt,
      criteria_names = c("CREATININE", "PLATELETS"), window_hours = 2,
      join_key = "PAT_ID"),
    "Need to specify time_var"
  )
  expect_error(
    constellate_criteria(crea_testpt, plts_testpt, window_hours = 2,
      join_key = "PAT_ID", time_var = "RECORDED_TIME"),
    "Need to provide criteria_names"
  )
  expect_error(
    constellate_criteria(crea_testpt, plts_testpt,
      criteria_names = c("CREATININE", "PLATELETS"), window_hours = 2,
      join_key = "PAT_ID", time_var = "RECORDED_TIME", value = "result"),
    "Need to specify result_var"
  )

  ## Arguments don't match
  expect_error(
    constellate_criteria(crea_testpt, plts_testpt,
        criteria_names = c("CREATININE", "PLATELETS"), window_hours = 2,
        join_key = "PAT_ID", time_var = "RECORDED_TIME", value = "foo"),
    "'arg' should be one of"
  )

  ## Appropriate classes and values
  expect_error(
    constellate_criteria("foo", plts_testpt,
      criteria_names = c("CREATININE", "PLATELETS"), window_hours = 2,
      join_key = "PAT_ID", time_var = "RECORDED_TIME"),
    "Need to pass only data frames in first argument"
  )
  expect_error(
    constellate_criteria(crea_testpt, plts_testpt,
      criteria_names = c(2, 2), window_hours = 2,
      join_key = "PAT_ID", time_var = "RECORDED_TIME"),
    "All criteria_names must be strings"
  )
  expect_error(
    constellate_criteria(crea_testpt, plts_testpt,
      criteria_names = c("CREATININE", "PLATELETS"), window_hours = "2",
      join_key = "PAT_ID", time_var = "RECORDED_TIME"),
    "All window_hours must be numeric"
  )

  ## Same number of window hours as data frames
  expect_error(
    constellate_criteria(crea_testpt, plts_testpt,
      criteria_names = c("CREATININE", "PLATELETS"), window_hours = c(2, 2, 2),
      join_key = "PAT_ID", time_var = "RECORDED_TIME"),
    paste0("Need to pass a single window hour length for all criteria data",
         " frames or a window hour length for each criteria data frame.")
  )

  ## Join key and time variable missing
  expect_error(
    constellate_criteria(crea_testpt, plts_testpt,
      criteria_names = c("CREATININE", "PLATELETS"), window_hours = 2,
      join_key = "foo", time_var = "RECORDED_TIME"),
    "'join_key' is not a column name in all time series data frames"
  )
  expect_error(
    constellate_criteria(crea_testpt, plts_testpt,
      criteria_names = c("CREATININE", "PLATELETS"), window_hours = 2,
      join_key = "PAT_ID", time_var = "foo"),
    "'time_var' is not a column name in all time series data frames"
  )

  ## Time variable in events data frame not POSIXct
  crea_testpt[, RECORDED_TIME := as.Date(RECORDED_TIME)]
  expect_error(
    constellate_criteria(crea_testpt, plts_testpt,
      criteria_names = c("CREATININE", "PLATELETS"), window_hours = 2,
      join_key = "PAT_ID", time_var = "RECORDED_TIME"),
    "'time_var' column in all time series data frames must be POSIXct class"
  )
  crea_testpt <- labs[VARIABLE == "PLATELETS" & PAT_ID == "108546"]
  crea_testpt <- crea_testpt[, RECORDED_TIME :=
                                 as.POSIXct(RECORDED_TIME, tz = "UTC")]

  ## Same number of criteria names as data frames
  expect_error(
    constellate_criteria(crea_testpt, plts_testpt,
      criteria_names = c("PLATELETS"), window_hours = 2, join_key = "PAT_ID",
      time_var = "RECORDED_TIME"),
    paste0("Need to pass a name for each criteria data frame. The number of",
      " data frames does not equal the number of names.")
  )

  ## result_var is present in all data frames if selected
  expect_error(
    constellate_criteria(crea_testpt, plts_testpt,
      criteria_names = c("CREATININE", "PLATELETS"), window_hours = 2,
      join_key = "PAT_ID", time_var = "RECORDED_TIME", value = "result",
      result_var = "foo"),
    "'result_var' is not a column name in all time series data frames"
  )
})
