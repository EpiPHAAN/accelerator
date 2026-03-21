# Compute Mean and SD of Duration

Computes the mean and standard deviation of durations across all valid
intervals. Automatically initializes a `.valid` flag if none exists.

## Usage

``` r
aggregate2_Duration_meansd(df, ...)
```

## Arguments

- df:

  The data frame with an evaluated `dur` numeric column.

- ...:

  Additional arguments.

## Value

A 1-row data frame with `mean_dur` and `sd_dur`.
