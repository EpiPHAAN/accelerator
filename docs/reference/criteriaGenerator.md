# Helps create functions for evaluating boolean expressions over a data frame that are going to be used as criteria for searching bouts.

Helps create functions for evaluating boolean expressions over a data
frame that are going to be used as criteria for searching bouts.

## Usage

``` r
criteriaGenerator(EXPRESSION, useNW = TRUE)
```

## Arguments

- EXPRESSION:

  A string representing an R logical expression (e.g., `"ENMO > 0.05"`).
  This expression will be evaluated dynamically across the rows of the
  data frame.

- useNW:

  Logical. If TRUE, the generated function will automatically enforce a
  non-wear condition (`!.criterioNW`) to ensure intervals are true wear
  time. Default is TRUE.

## Value

A function that takes a data frame with accelerometry data and returns a
vector of booleans
