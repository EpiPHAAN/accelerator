# Create a summery for the column .criterio of a data frame representing accelerometer data

Generates a summary for a epoch or BIN file

Aggregates accelerometer data by day to produce summaries of wear time,
non-wear time, and valid days. Computes overall averages and weighted
statistics.

## Usage

``` r
getSummary(
  df,
  offset = dhours(0),
  minimoHorasValidas = 20,
  maximoHorasNonWear = 2,
  durBoutMin = dseconds(5)
)

getSummary(
  df,
  offset = dhours(0),
  minimoHorasValidas = 20,
  maximoHorasNonWear = 2,
  durBoutMin = dseconds(5)
)
```

## Arguments

- df:

  A data frame containing accelerometer data with a `timestamp` column,
  alongside logical criteria like `.criterioRaw`, `.criterioBout`, and
  `.criterioNW`.

- offset:

  A `lubridate::Duration` offset added to the timestamps to shift the
  definition of a "day". Default is 0.

- minimoHorasValidas:

  Minimum number of valid 'wear' hours required for a day to be
  considered valid. Default is 20.

- maximoHorasNonWear:

  Maximum number of allowed 'non-wear' hours per day. Default is 2.

- durBoutMin:

  Minimum duration required to compute valid intervals using
  `criterio2Interval`. Default is 5 seconds.

## Value

a list with a summary. This is the info that we use to define activity
variables on a daily basis and on a global value.

A list containing: `dailyTable` (the daily summary tibble), `average`
(simple average of sum), `weightedaverage` (weighted by valid wear
hours), `totalValidHours`, and `intervals`.
