# Aggregate Activity Duration

Computes the accumulated duration of activity intervals grouped by their
generic string identifier or name.

## Usage

``` r
aggregate_Duration(df, units = "secs", ...)
```

## Arguments

- df:

  The data frame containing interval data with `start`, `end`, and
  `name` columns.

- units:

  The corresponding time units to output (e.g. `"secs"`, `"mins"`).
  Default is `"secs"`.

- ...:

  Additional arguments.

## Value

A summary data frame with columns: `name` and `dur` (total duration).
