# Convert an interval to a dataframe

Parses
[`lubridate::interval`](https://lubridate.tidyverse.org/reference/interval.html)
objects or a nested list of intervals into a standardized `start`/`end`
data frame. Can optionally assign custom identifier columns.

## Usage

``` r
interval2Dataframe(intervals, .id = c())
```

## Arguments

- intervals:

  A lubridate interval vector or list containing intervals.

- .id:

  An optional character vector specifying column names to dynamically
  bind when unnesting lists.

## Value

A data frame containing `start` and `end` representations of the
provided intervals.
