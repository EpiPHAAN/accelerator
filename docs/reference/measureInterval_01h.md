# Generate Hourly Named Intervals

Creates a sequence of continuous hourly intervals covering the entire
timespan of the provided data frame.

## Usage

``` r
measureInterval_01h(
  df,
  first = TRUE,
  last = TRUE,
  offsetLabels = dhours(0),
  starts = dhours(0),
  duration = period(1, "hours")
)
```

## Arguments

- df:

  A data frame representing the accelerometry data with a `timestamp`
  column.

- first:

  Logical. If TRUE, includes the first boundary interval. Default is
  TRUE.

- last:

  Logical. If TRUE, includes the last boundary interval. Default is
  TRUE.

- offsetLabels:

  An offset added to the labels (names) of the generated intervals.
  Default is 0 hours.

- starts:

  A duration shift applied to the start times. Default is 0 hours.

- duration:

  The duration of each generated interval block. Default is 1 hour
  (`period(1, "hours")`).

## Value

A named lubridate interval vector containing the hourly windows.
