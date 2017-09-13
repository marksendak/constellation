
<!-- README.md is generated from README.Rmd. Please edit that file -->
Overview
--------

Constellation contains a set of functions for applying multidimensional, time window based logic to time series data frames of arbitrary length. Constellation was developed to enable rapid and flexible identification of series of events that occur in hospitalized patients. The functions have been abstracted for general purpose use with time series data. Constellation extends and provides a friendly API to rolling joins and overlap joins implemented in [data.table](https://cran.r-project.org/web/packages/data.table/data.table.pdf).

There are three functions included in constellation to build complex features from time series data:

-   `value_change()` identify increases or decreases in a value within a given time window
-   `constellate()` identify time stamps when a series of events occur within a given time window
-   `constellate_criteria()` identify which events occur within a given time window for every measurement time stamp

Constellation can be used to build point-based scores for time series data, identify particular sequences of events that occur near each other, and identify when specific changes occur for a given parameter.

If you are new to constellation, the best place to start is the `vignette("constellation", "identify_sepsis")`.

Installation
------------

You can install constellation from github with:

``` r
devtools::install_github("marksendak/constellation")
#> Skipping install of 'constellation' from a github remote, the SHA1 (20099891) has not changed since last install.
#>   Use `force = TRUE` to force installation
```

This package is under development in preparation of release on CRAN. If you have any questions, comments, or feedback, please email <mark.sendak@gmail.com>.

Example
-------

This is a basic example which shows you how to solve a common problem:

``` r
library(constellation)
library(fasttime)

systolic_bp <- vitals[VARIABLE == "SYSTOLIC_BP"]
systolic_bp[, RECORDED_TIME := fastPOSIXct(RECORDED_TIME)]
head(systolic_bp)
#>    PAT_ID       RECORDED_TIME    VALUE    VARIABLE
#> 1: 108546 2010-02-25 00:36:15 110.6677 SYSTOLIC_BP
#> 2: 108546 2010-02-25 03:41:56 116.0423 SYSTOLIC_BP
#> 3: 108546 2010-02-25 05:30:53 119.2235 SYSTOLIC_BP
#> 4: 108546 2010-02-25 06:05:43 102.9899 SYSTOLIC_BP
#> 5: 108546 2010-02-25 06:48:29 122.1348 SYSTOLIC_BP
#> 6: 108546 2010-02-25 07:14:18 119.7529 SYSTOLIC_BP

systolic_bp_drop <- value_change(systolic_bp, value = 40, direction = "down",
    window_hours = 6, join_key = "PAT_ID", time_var = "RECORDED_TIME", 
    value_var = "VALUE", mult = "first")
head(systolic_bp_drop)
#>    PAT_ID PRIOR_RECORDED_TIME PRIOR_VALUE CURRENT_RECORDED_TIME
#> 1: 108546 2010-02-25 10:45:29    139.9967   2010-02-25 15:42:35
#> 2: 112374 2010-03-09 13:18:13    160.4919   2010-03-09 15:48:09
#> 3: 113163 2010-07-27 11:50:35    170.2034   2010-07-27 15:21:58
#> 4: 124042 2010-11-24 16:34:57    163.8912   2010-11-24 21:03:14
#> 5: 135995 2010-11-20 20:51:09    157.9432   2010-11-20 22:26:00
#> 6: 146478 2010-08-27 12:07:47    179.3603   2010-08-27 17:03:05
#>    CURRENT_VALUE
#> 1:      80.07446
#> 2:     107.87212
#> 3:     116.22419
#> 4:     116.66625
#> 5:     111.55469
#> 6:     132.99234

systolic_bp_drop <- value_change(systolic_bp, value = 40, direction = "down",
    window_hours = 6, join_key = "PAT_ID", time_var = "RECORDED_TIME", 
    value_var = "VALUE", mult = "last")
head(systolic_bp_drop)
#>    PAT_ID PRIOR_RECORDED_TIME PRIOR_VALUE CURRENT_RECORDED_TIME
#> 1: 108546 2010-07-01 11:31:31    164.9851   2010-07-01 17:03:04
#> 2: 112374 2010-03-15 11:15:53    164.1634   2010-03-15 13:30:06
#> 3: 113163 2010-07-30 15:12:15    160.1682   2010-07-30 18:33:10
#> 4: 124042 2010-12-04 13:34:18    167.2564   2010-12-04 17:46:57
#> 5: 135995 2010-11-26 23:47:15    127.5603   2010-11-27 01:43:05
#> 6: 146478 2010-09-03 11:14:43    182.1690   2010-09-03 12:18:28
#>    CURRENT_VALUE
#> 1:     114.67968
#> 2:     115.95783
#> 3:     111.89387
#> 4:     118.81151
#> 5:      81.90537
#> 6:     138.28222

systolic_bp_drop <- value_change(systolic_bp, value = 40, direction = "down",
    window_hours = 6, join_key = "PAT_ID", time_var = "RECORDED_TIME", 
    value_var = "VALUE", mult = "all")
head(systolic_bp_drop)
#>    PAT_ID PRIOR_RECORDED_TIME PRIOR_VALUE CURRENT_RECORDED_TIME
#> 1: 108546 2010-02-25 10:45:29    139.9967   2010-02-25 15:42:35
#> 2: 108546 2010-03-01 10:57:24    136.8654   2010-03-01 11:07:00
#> 3: 108546 2010-03-02 14:59:20    129.0167   2010-03-02 19:46:35
#> 4: 108546 2010-03-02 15:49:00    110.1830   2010-03-02 19:46:35
#> 5: 108546 2010-03-03 19:18:41    137.8095   2010-03-03 23:23:54
#> 6: 108546 2010-03-03 21:13:39    130.3280   2010-03-03 23:23:54
#>    CURRENT_VALUE
#> 1:      80.07446
#> 2:      88.88972
#> 3:      69.94551
#> 4:      69.94551
#> 5:      82.16874
#> 6:      82.16874
```

Why constellation?
------------------

In clinical medicine, there are a subset of conditions that are defined by a sequence of related events that unfold over time. These conditions are described as a "*constellation of signs and symptoms*."

Duke Institute for Health Innovation
------------------------------------

constellation was originally developed to support a machine learning project at the [Duke Institute for Health Innovation](http://www.dihi.org/) to predict sepsis.
