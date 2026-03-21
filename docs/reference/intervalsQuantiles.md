# Calculate Temporal Quantiles of Intervals

Computes cumulative temporal quantiles across a collection of intervals.
It determines the exact timestamp where the specified duration
proportions (`probs`) of the total interval time are reached.

## Usage

``` r
intervalsQuantiles(intervals, probs = c(0.5))
```

## Arguments

- intervals:

  A data frame representing the intervals (with `from` and `to`
  columns).

- probs:

  A numeric vector of probabilities (quantiles) with values between 0
  and 1. Default is 0.5 (median).

## Value

A data frame containing the corresponding timestamp quantiles for the
overall duration of the provided intervals.
