# Connect Clustered Time Intervals

Merges consecutive or close time intervals into larger continuous bouts
if the temporal gap between them is smaller than a specified absolute or
relative threshold.

## Usage

``` r
connectInterval(interval, distance = dminutes(0), distanceRel = NULL)
```

## Arguments

- interval:

  A data frame containing time intervals, usually with `start` and `end`
  POSIXct columns.

- distance:

  A `lubridate::Duration` or time span object. Intervals separated by a
  gap smaller than this absolute distance will be connected into a
  single interval. Default is 0 minutes.

- distanceRel:

  A numeric value. Connects intervals if the gap is smaller than a
  proportion (`distanceRel`) of the duration of the current or following
  interval. Default is NULL.

## Value

A data frame with the connected/merged time intervals (`start` and
`end`).
