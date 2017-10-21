library(constellation)
context("Constellate and Show Criteria")

## Build test patient
crea_testpt <- labs[VARIABLE == "CREATININE" & PAT_ID == "108546"]
plts_testpt <- labs[VARIABLE == "PLATELETS" & PAT_ID == "108546"]

## Tests
test_that("constellate criteria produces expected values for test patient", {
  ####### test lab orders without final event
  crea_plts <- rbind(
      data.table(PAT_ID = 108546, RECORDED_TIME = 
        fastPOSIXct("2010-02-25 10:27:44", tz = "GMT"), CREATININE = 0, 
        PLATELETS = 1),
      data.table(PAT_ID = 108546, RECORDED_TIME = 
        fastPOSIXct("2010-02-26 01:48:18", tz = "GMT"), CREATININE = 1,
        PLATELETS = 0),
      data.table(PAT_ID = 108546, RECORDED_TIME = 
        fastPOSIXct("2010-02-26 14:36:46", tz = "GMT"), CREATININE = 0,
        PLATELETS = 1)
      )
  crea_plts <- setkeyv(crea_plts, c("PAT_ID", "RECORDED_TIME"))

  ## Test
  expect_equal(head(constellate_criteria(crea_testpt, plts_testpt,
    criteria_names = c("CREATININE", "PLATELETS"), window_hours = 2,
    join_key = "PAT_ID", time_var = "RECORDED_TIME"), n = 3), crea_plts)

  ####### test lab orders with final event
  crea_plts <- rbind(
      data.table(PAT_ID = 108546, RECORDED_TIME =
        fastPOSIXct("2010-02-25 10:27:44", tz = "GMT"), CREATININE = 0,
        PLATELETS = 1, FINAL_EVENT = "PLATELETS"),
      data.table(PAT_ID = 108546, RECORDED_TIME =
        fastPOSIXct("2010-02-26 01:48:18", tz = "GMT"), CREATININE = 1,
        PLATELETS = 0, FINAL_EVENT = "CREATININE"),
      data.table(PAT_ID = 108546, RECORDED_TIME =
        fastPOSIXct("2010-02-26 14:36:46", tz = "GMT"), CREATININE = 0,
        PLATELETS = 1, FINAL_EVENT = "PLATELETS")
      )
  crea_plts <- setkeyv(crea_plts, c("PAT_ID", "RECORDED_TIME"))

  ## Test
  expect_equal(head(constellate_criteria(crea_testpt, plts_testpt,
    criteria_names = c("CREATININE", "PLATELETS"), window_hours = 2,
    join_key = "PAT_ID", time_var = "RECORDED_TIME", final_event = TRUE),
    n = 3), crea_plts)
})

test_that("column names assign properly", {
  ####### test lab orders without final event
  crea_plts <- rbind(
      data.table(PAT_ID = 108546, RECORDED_TIME =
        fastPOSIXct("2010-02-25 10:27:44", tz = "GMT"), LAB_1 = 0, LAB_2 = 1),
      data.table(PAT_ID = 108546, RECORDED_TIME =
        fastPOSIXct("2010-02-26 01:48:18", tz = "GMT"), LAB_1 = 1, LAB_2 = 0),
      data.table(PAT_ID = 108546, RECORDED_TIME =
        fastPOSIXct("2010-02-26 14:36:46", tz = "GMT"), LAB_1 = 0, LAB_2 = 1)
  )
  crea_plts <- setkeyv(crea_plts, c("PAT_ID", "RECORDED_TIME"))

  ## Test
  expect_equal(head(constellate_criteria(crea_testpt, plts_testpt,
    criteria_names = c("LAB_1", "LAB_2"), window_hours = 2,
    join_key = "PAT_ID", time_var = "RECORDED_TIME"), n = 3), crea_plts)
})

test_that("final column added correctly", {
  expect_equal(ncol(constellate_criteria(crea_testpt, plts_testpt,
    criteria_names = c("CREATININE", "PLATELETS"), window_hours = 2,
    join_key = "PAT_ID", time_var = "RECORDED_TIME")), 4)
  expect_equal(ncol(constellate_criteria(crea_testpt, plts_testpt,
    criteria_names = c("CREATININE", "PLATELETS"), window_hours = 2,
    join_key = "PAT_ID", time_var = "RECORDED_TIME", final_event = TRUE)), 5)
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
    "Need to specify join key"
  )
  expect_error(
    constellate_criteria(crea_testpt, plts_testpt,
      criteria_names = c("CREATININE", "PLATELETS"), window_hours = 2,
      join_key = "PAT_ID"),
    "Need to specify time variable"
  )
  expect_error(
    constellate_criteria(crea_testpt, plts_testpt, window_hours = 2,
      join_key = "PAT_ID", time_var = "RECORDED_TIME"),
    "Need to provide criteria names"
  )

  ## Appropriate classes and values
  expect_error(
    constellate_criteria(crea_testpt, plts_testpt,
      criteria_names = c("CREATININE", "PLATELETS"), window_hours = "2",
      join_key = "PAT_ID", time_var = "RECORDED_TIME"),
    "All window_hours must be numeric"
  )
  expect_error(
    constellate_criteria(crea_testpt, plts_testpt,
      criteria_names = c("CREATININE", "PLATELETS"), window_hours = -5,
      join_key = "PAT_ID", time_var = "RECORDED_TIME"),
    "All window_hours must be greater than 0"
  )
  expect_error(
    constellate_criteria("foo", plts_testpt,
      criteria_names = c("CREATININE", "PLATELETS"), window_hours = 2,
      join_key = "PAT_ID", time_var = "RECORDED_TIME"),
    "Need to pass only data frames in first argument"
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

  ## Same number of criteria names and window hours as data frames
  expect_error(
    constellate_criteria(crea_testpt, plts_testpt,
      criteria_names = c("PLATELETS"), window_hours = 2, join_key = "PAT_ID",
      time_var = "RECORDED_TIME"),
    paste0("Need to pass a name for each criteria data frame. The number of",
      " data frames does not equal the number of names.")
  )
  expect_error(
    constellate_criteria(crea_testpt, plts_testpt,
      criteria_names = c("CREATININE", "PLATELETS"), window_hours = c(2, 2, 2),
      join_key = "PAT_ID", time_var = "RECORDED_TIME"),
    paste0("Need to pass a single window hour length for all criteria data",
      " frames or a window hour length for each criteria data frame.")
  )
})
