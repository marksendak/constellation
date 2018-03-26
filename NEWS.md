Development Version
===================

## NEW FEATURES
* New `incidents()` function to separate distinct illness episodes into incident events

## CHANGES

* Fixed window_hours for blood pressure drop in sepsis definition
* Removed fasttime dependency. Constellation functions no longer do any date time conversions and will throw errors if timestamp variables aren't passed to functions as POSIXct class.

Version 0.1.0
=============

## NEW FEATURES

* New `value` argument in `constellate_criteria()` function to increase flexibility in populating criteria columns
* New `bundle()` function to calculate time of treatment bundle

## CHANGES

* Removed error handling for negative `window_hours`

## BUG FIXES

* Remove github_install() command calls from Identifying Sepsis vignette to align with CRAN policies
