# Correct Timestamps for Daylight Saving Time (DST) Transitions

Adjusts the timestamps in a data frame to prevent gaps or overlaps
caused by summer/winter time changes. It safely handles 3600-second
offsets if a DST transition occurs within the dataset.

## Usage

``` r
correctForDST(dfBB)
```

## Arguments

- dfBB:

  A data frame containing a `timestamp` column (POSIXct objects).

## Value

The mutated data frame with corrected \`timestamp\` values to guarantee
a continuous time series.
