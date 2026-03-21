# Generate Daily Named Intervals

Creates a sequence of continuous 24-hour intervals covering the timespan
of the provided data frame, aligned with midnight.

## Usage

``` r
measureInterval_24h(
  df,
  first = TRUE,
  last = TRUE,
  offsetLabels = dhours(0),
  starts = dhours(0),
  duration = period(1, "days"),
  .isOn = NULL,
  withName = TRUE
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

  An offset added to the daily labels (names). Default is 0 hours.

- starts:

  A duration offset applied to the start times. Default is 0 hours.

- duration:

  The duration of each generated interval block. Default is 1 day
  (`period(1, "days")`).

- .isOn:

  Optional explicit interval bounds to delimit the sequence generation.

- withName:

  Logical. If TRUE, assigns formatted date string names to each
  interval. Default is TRUE.

## Value

A named (or unnamed) lubridate interval vector containing the daily
windows.
