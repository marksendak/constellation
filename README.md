
<!-- README.md is generated from README.Rmd. Please edit that file -->

# constellation

[![Build
Status](https://travis-ci.org/marksendak/constellation.svg?branch=master)](https://travis-ci.org/marksendak/constellation)
[![Windows build
status](https://ci.appveyor.com/api/projects/status/github/marksendak/constellation?branch=master&svg=true)](https://ci.appveyor.com/project/marksendak/constellation)

## Overview

Constellation contains a set of functions for applying multidimensional,
time window based logic to time series data frames of arbitrary length.
Constellation was developed to enable rapid and flexible identification
of series of events that occur in hospitalized patients. The functions
have been abstracted for general purpose use with time series data.
Constellation extends and provides a friendly API to rolling joins and
overlap joins implemented in
[data.table](https://CRAN.R-project.org/package=data.table). Three
datasets (labs, vitals, and orders) with randomly synthesized time
series data for a cohort of 100 patients are included to facilitate
testing of functions.

There are five functions included in constellation to build complex
features from time series data:

  - `value_change()` identify increases or decreases in a value within a
    given time window
  - `constellate()` identify time stamps when a series of events occurs
    within a given time window
  - `constellate_criteria()` identify which events occur within a given
    time window for every measurement time stamp
  - `bundle()` identify which events occur within a given time window of
    a given event
  - `incidents()` identify distinct, incident episodes that must be
    separated in time by a minimum of a given time window

The `constellate_criteria()` and `bundle()` function are similar, but
the `bundle()` function is anchored around a specific event table. The
`bundle()` function identifies events that occur within a given time
window of a **specific** event data frame that is supplied to the
function. On the other hand, the `constellate_criteria()` function
identifies events that occur within a given time window of **any** event
data frame that is supplied to the function. The first data frame passed
to the `bundle()` function is used as an anchor to search through the
subsequent data frames passed to the function. The order of data frames
is significant and passing different data frames as the first argument
will generate different results. On the other hand, the order in which
you pass data frames to the `constellate_criteria()` function is
insignificant. Passing data frames in different orders will generate
equivalent results.

Constellation can be used to build point-based scores for time series
data (via `constellate_criteria()`), identify particular sequences of
events that occur near each other (via `constellate()`), identify when
specific changes occur for a given parameter (via `value_change()`),
identify individual events that occur around a specified time stamp (via
`bundle()`), and distinguish between eveents that are separated by a
specified time window (via `incidents()`).

If you are new to constellation, the best place to start is the
`vignette("constellation", "identify_sepsis")`. You can also view the
sepsis vignette on
[CRAN](https://cran.r-project.org/package=constellation/vignettes/identify_sepsis.html).

## Installation

You can install constellation from CRAN with:

``` r
install.packages("constellation")
library(constellation)
```

You can install the development version of constellation from github
with:

``` r
devtools::install_github("marksendak/constellation")
```

If you have any questions, comments, or feedback, please email
<mark.sendak@gmail.com>.

## Example

Below are several variations of finding systolic blood pressure drops of
40 over a 6 hour period.

Examine systolic blood pressure data:

``` r
library(constellation)

systolic_bp <- vitals[VARIABLE == "SYSTOLIC_BP"]
systolic_bp[, RECORDED_TIME := as.POSIXct(RECORDED_TIME, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")]
head(systolic_bp)
#>    PAT_ID       RECORDED_TIME    VALUE    VARIABLE
#> 1: 108546 2010-02-25 05:36:15 110.6677 SYSTOLIC_BP
#> 2: 108546 2010-02-25 08:41:56 116.0423 SYSTOLIC_BP
#> 3: 108546 2010-02-25 10:30:53 119.2235 SYSTOLIC_BP
#> 4: 108546 2010-02-25 11:05:43 102.9899 SYSTOLIC_BP
#> 5: 108546 2010-02-25 11:48:29 122.1348 SYSTOLIC_BP
#> 6: 108546 2010-02-25 12:14:18 119.7529 SYSTOLIC_BP
```

Identify the first systolic blood pressure drop per
patient:

``` r
systolic_bp_drop <- value_change(systolic_bp, value = 40, direction = "down",
    window_hours = 6, join_key = "PAT_ID", time_var = "RECORDED_TIME", 
    value_var = "VALUE", mult = "first")
head(systolic_bp_drop)
#>    PAT_ID PRIOR_RECORDED_TIME PRIOR_VALUE CURRENT_RECORDED_TIME
#> 1: 108546 2010-02-25 15:45:29    139.9967   2010-02-25 20:42:35
#> 2: 112374 2010-03-09 18:18:13    160.4919   2010-03-09 20:48:09
#> 3: 113163 2010-07-27 15:50:35    170.2034   2010-07-27 19:21:58
#> 4: 124042 2010-11-24 21:34:57    163.8912   2010-11-25 02:03:14
#> 5: 135995 2010-11-21 01:51:09    157.9432   2010-11-21 03:26:00
#> 6: 146478 2010-08-27 16:07:47    179.3603   2010-08-27 21:03:05
#>    CURRENT_VALUE
#> 1:      80.07446
#> 2:     107.87212
#> 3:     116.22419
#> 4:     116.66625
#> 5:     111.55469
#> 6:     132.99234
```

Identify the last systolic blood pressure drop per
patient:

``` r
systolic_bp_drop <- value_change(systolic_bp, value = 40, direction = "down",
    window_hours = 6, join_key = "PAT_ID", time_var = "RECORDED_TIME", 
    value_var = "VALUE", mult = "last")
head(systolic_bp_drop)
#>    PAT_ID PRIOR_RECORDED_TIME PRIOR_VALUE CURRENT_RECORDED_TIME
#> 1: 108546 2010-07-01 15:31:31    164.9851   2010-07-01 21:03:04
#> 2: 112374 2010-03-15 15:15:53    164.1634   2010-03-15 17:30:06
#> 3: 113163 2010-07-30 19:12:15    160.1682   2010-07-30 22:33:10
#> 4: 124042 2010-12-04 18:34:18    167.2564   2010-12-04 22:46:57
#> 5: 135995 2010-11-27 04:47:15    127.5603   2010-11-27 06:43:05
#> 6: 146478 2010-09-03 15:14:43    182.1690   2010-09-03 16:18:28
#>    CURRENT_VALUE
#> 1:     114.67968
#> 2:     115.95783
#> 3:     111.89387
#> 4:     118.81151
#> 5:      81.90537
#> 6:     138.28222
```

Identify all systolic blood pressure drops per
patient:

``` r
systolic_bp_drop <- value_change(systolic_bp, value = 40, direction = "down",
    window_hours = 6, join_key = "PAT_ID", time_var = "RECORDED_TIME", 
    value_var = "VALUE", mult = "all")
head(systolic_bp_drop)
#>    PAT_ID PRIOR_RECORDED_TIME PRIOR_VALUE CURRENT_RECORDED_TIME
#> 1: 108546 2010-02-25 15:45:29    139.9967   2010-02-25 20:42:35
#> 2: 108546 2010-03-01 15:57:24    136.8654   2010-03-01 16:07:00
#> 3: 108546 2010-03-02 19:59:20    129.0167   2010-03-03 00:46:35
#> 4: 108546 2010-03-02 20:49:00    110.1830   2010-03-03 00:46:35
#> 5: 108546 2010-03-04 00:18:41    137.8095   2010-03-04 04:23:54
#> 6: 108546 2010-03-04 02:13:39    130.3280   2010-03-04 04:23:54
#>    CURRENT_VALUE
#> 1:      80.07446
#> 2:      88.88972
#> 3:      69.94551
#> 4:      69.94551
#> 5:      82.16874
#> 6:      82.16874
```

## Why constellation?

In clinical medicine, there are a subset of conditions that are defined
by a sequence of related events that unfold over time. These conditions
are described as a “*constellation of signs and symptoms*.”

Another piece of medical jargon that made it into the package is the
concept of a treatment bundle. The `bundle()` function was originally
designed to calculate the time stamp at which a group of treatments is
delivered for every patient within a specified amount of time of
developing a condition.

## Duke Institute for Health Innovation

constellation was originally developed to support a machine learning
project at the [Duke Institute for Health
Innovation](http://www.dihi.org/) to predict sepsis.
