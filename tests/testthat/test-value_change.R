library(constellation)
context("Detecting Value Changes")

## Build test patient
sbp_testpt <- vitals[VARIABLE == "SYSTOLIC_BP" & PAT_ID == "108546"]
sbp_testpt[, VALUE := round(VALUE, digits = 4)]

## Tests
test_that("value change produces expected values for test patient", {
  ####### First drop
  ## First drop
  first_drop <- data.table(PAT_ID = 108546, PRIOR_RECORDED_TIME = 
    fastPOSIXct("2010-02-25 15:45:29", tz = "GMT"), PRIOR_VALUE = 139.9967, 
    CURRENT_RECORDED_TIME = fastPOSIXct("2010-02-25 20:42:35", tz = "GMT"), 
    CURRENT_VALUE = 80.0745)
  first_drop <- setkey(first_drop, "PAT_ID")

  ## Test
  expect_equal(value_change(sbp_testpt, value = 40, direction = "down", 
    window_hours = 6, join_key = "PAT_ID", time_var = "RECORDED_TIME", 
    value_var = "VALUE", mult = "first"), first_drop)

  ####### Last drop
  ## Last drop
  last_drop <- data.table(PAT_ID = 108546, PRIOR_RECORDED_TIME = 
    fastPOSIXct("2010-07-01 15:31:31", tz = "GMT"), PRIOR_VALUE = 164.9851, 
    CURRENT_RECORDED_TIME = fastPOSIXct("2010-07-01 21:03:04", tz = "GMT"), 
    CURRENT_VALUE = 114.6797)
  last_drop <- setkey(last_drop, "PAT_ID")

  ## Test
  expect_equal(value_change(sbp_testpt, value = 40, direction = "down", 
    window_hours = 6, join_key = "PAT_ID", time_var = "RECORDED_TIME", 
    value_var = "VALUE", mult = "last"), last_drop)

  ####### All drops - check rownum, not entries
  expect_equal(nrow(value_change(sbp_testpt, value = 40, direction = "down", 
    window_hours = 6, join_key = "PAT_ID", time_var = "RECORDED_TIME", 
    value_var = "VALUE", mult = "all")), 188)

  ####### Remove files
  rm(last_drop, first_drop)
})

test_that("default arguments function properly", {
  ####### Function output is identical when you drop the mult and direction 
  # arguments vs set them to "all"
  expect_equal(value_change(sbp_testpt, value = 40, direction = "down", 
    window_hours = 6, join_key = "PAT_ID", time_var = "RECORDED_TIME", 
    value_var = "VALUE", mult = "all"), value_change(sbp_testpt, value = 40, 
    direction = "down", window_hours = 6, join_key = "PAT_ID", 
    time_var = "RECORDED_TIME", value_var = "VALUE"))
  expect_equal(value_change(sbp_testpt, value = 40, direction = "all", 
    window_hours = 6, join_key = "PAT_ID", time_var = "RECORDED_TIME", 
    value_var = "VALUE", mult = "first"), value_change(sbp_testpt, 
    value = 40, window_hours = 6, join_key = "PAT_ID", 
    time_var = "RECORDED_TIME", value_var = "VALUE", mult = "first"))
  expect_equal(value_change(sbp_testpt, value = 40, direction = "all", 
    window_hours = 6, join_key = "PAT_ID", time_var = "RECORDED_TIME", 
    value_var = "VALUE", mult = "all"), value_change(sbp_testpt, value = 40,
    window_hours = 6, join_key = "PAT_ID", time_var = "RECORDED_TIME", 
    value_var = "VALUE"))
})

test_that("error messages function", {
  ## Missing arguments
  expect_error(value_change(value = 40, direction = "all", window_hours = 6,
    join_key = "PAT_ID", time_var = "RECORDED_TIME", value_var = "VALUE", 
    mult = "all"), "Need to pass data frame")
  expect_error(value_change(sbp_testpt, direction = "all", window_hours = 6,
    join_key = "PAT_ID", time_var = "RECORDED_TIME", value_var = "VALUE", 
    mult = "all"), "Need to specify change value")
  expect_error(value_change(sbp_testpt, value = 40, direction = "all", 
    join_key = "PAT_ID", time_var = "RECORDED_TIME", value_var = "VALUE", 
    mult = "all"), "Need to specify window hours")
  expect_error(value_change(sbp_testpt, value = 40, direction = "all", 
    window_hours = 6, time_var = "RECORDED_TIME", value_var = "VALUE", 
    mult = "all"), "Need to specify join key")
  expect_error(value_change(sbp_testpt, value = 40, direction = "all", 
    window_hours = 6, join_key = "PAT_ID", value_var = "VALUE", 
    mult = "all"), "Need to specify time variable")
  expect_error(value_change(sbp_testpt, value = 40, direction = "all", 
    window_hours = 6, join_key = "PAT_ID", time_var = "RECORDED_TIME", 
    mult = "all"), "Need to specify value variable")

  ## Args don't match
  expect_error(value_change(sbp_testpt, value = 40, direction = "foo", 
    window_hours = 6, join_key = "PAT_ID", time_var = "RECORDED_TIME", 
    value_var = "VALUE", mult = "all"), "'arg' should be one of")
  expect_error(value_change(sbp_testpt, value = 40, direction = "all", 
    window_hours = 6, join_key = "PAT_ID", time_var = "RECORDED_TIME", 
    value_var = "VALUE", mult = "foo"), "'arg' should be one of")

  ## Wrong class
  expect_error(value_change("foo", value = 40, direction = "all", 
    window_hours = 6, join_key = "PAT_ID", time_var = "RECORDED_TIME", 
    value_var = "VALUE", mult = "all"), "'data' must be a data.frame")
  expect_error(value_change(sbp_testpt, value = "foo", direction = "all", 
    window_hours = 6, join_key = "PAT_ID", time_var = "RECORDED_TIME", 
    value_var = "VALUE", mult = "all"), "'value' must be numeric")
  expect_error(value_change(sbp_testpt, value = 40, direction = "all", 
    window_hours = "foo", join_key = "PAT_ID", time_var = "RECORDED_TIME", 
    value_var = "VALUE", mult = "all"), "'window_hours' must be numeric")

  ## Missing column
  expect_error(value_change(sbp_testpt, value = 40, direction = "all", 
    window_hours = 6, join_key = "foo", time_var = "RECORDED_TIME", 
    value_var = "VALUE", mult = "all"),
    "'join_key' is not a column name in data")
  expect_error(value_change(sbp_testpt, value = 40, direction = "all", 
    window_hours = 6, join_key = "PAT_ID", time_var = "foo", 
    value_var = "VALUE", mult = "all"),
    "'time_var' is not a column name in data")
  expect_error(value_change(sbp_testpt, value = 40, direction = "all", 
    window_hours = 6, join_key = "PAT_ID", time_var = "RECORDED_TIME", 
    value_var = "foo", mult = "all"),
    "'value_var' is not a column name in data")
})
