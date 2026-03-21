# Find Touched Intervals

Identifies intervals from `what_n` that overlap or touch intervals in
`when_n`. Unlike intersection, this returns the original untouched
bounds of matching segments.

## Usage

``` r
touchedIntervals(what_n, when_n, useNames = FALSE, short = TRUE, operator = I)
```

## Arguments

- what_n:

  The primary interval vector or list.

- when_n:

  The target interval vector or list against which overlaps are checked.

- useNames:

  Logical. If TRUE, preserves/applies names.

- short:

  Logical. If TRUE, removes `NULL` (non-touching) intervals from the
  result.

- operator:

  A custom function/operator to apply over the touching intervals.
  Defaults to `I` (identity).

## Value

A consolidated vector/list of original intervals from `what_n` that
overlapped with `when_n`.
