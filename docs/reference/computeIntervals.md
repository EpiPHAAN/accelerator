# Compute Activity Intervals for Multiple Data Frames

Iterates over a data frame (typically a nested tibble) and applies the
`accelerometry2Intervals` function concurrently using `future_pmap`.

## Usage

``` r
computeIntervals(dataframe, defineIntervals)
```

## Arguments

- dataframe:

  A data frame, list, or grouped tibble containing columns that match
  the arguments expected by the `accelerometry2Intervals` driver.

- defineIntervals:

  A list of functions defining the intervals to be computed. Passed
  directly to `accelerometry2Intervals`.

## Value

A list containing the successfully computed intervals for each
row/element in the input `dataframe`.
