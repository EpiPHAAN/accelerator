# Evaluate Timeline Against Intervals

Projects a data frame of temporal intervals onto a fixed timeline array
(`ts`). Each point in the timeline array that falls within an interval
is assigned a boolean `TRUE`.

## Usage

``` r
interval2criterio(ts, intervalos)
```

## Arguments

- ts:

  A vector of POSIXct timestamps representing the chronological axis.

- intervalos:

  A data frame representing intervals, containing at minimum `from` and
  `to` columns.

## Value

A logical vector of the same length as `ts`, indicating if each
timestamp belongs to any interval.
