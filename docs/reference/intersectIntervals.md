# Intersect Interval Vectors or Lists

Computes the intersection of two vectors/lists of intervals. It supports
element-wise or list-based rigorous intersections of temporal ranges.

## Usage

``` r
intersectIntervals(what_n, when_n, useNames = FALSE, short = TRUE)
```

## Arguments

- what_n:

  The first interval vector or list of intervals.

- when_n:

  The second interval vector or list of intervals to intersect against
  `what_n`.

- useNames:

  Logical. If TRUE, attempts to preserve/apply names during the
  intersection using elements of `when_n`.

- short:

  Logical. If TRUE, filters out any resulting `NULL` elements that
  signify no intersection.

## Value

A vector or list containing the intersecting bounds of the provided
intervals.
