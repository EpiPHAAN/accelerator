# Indicates which rows meets the criteria for be considered part of a Bout meeting certain criteria

Generates a vector that indicates for each row of a dataframe (usually
epoch or BIN file) if that row meets the criteria to be considered part
of a Bout (veryfy a criteria for ca percent of time of a minimum
duration),

## Usage

``` r
criterioBout(df, pctBouts = 1, durBoutMin = dseconds(5))
```

## Arguments

- df:

  data frame with columns .criterio and eventually .criterioNW (that
  represents NonWear time as TRUE/FALSE).

- pctBouts:

  Numeric value between 0 and 1. Determines the minimum proportion or
  percentage of time within the window that the criteria must be
  fulfilled to be treated as a valid Bout.

- durBoutMin:

  Minimum amount of time (as a lubridate duration) that the condition
  must consistently be met to be considered a Bout.

## Value

A boolean vector (TRUE/FALSE) indicating if the condition of belonging
to a Bout is met for each corresponding epoch.
