# Evaluate SIB (Sustained Inactivity Bouts) v2

Computes Sustained Inactivity Bouts (SIB) based on a specific Z-angle
deviation threshold and a minimum bout duration.

## Usage

``` r
SIB_v2(df, critAnglez = 5, durBoutMin = dminutes(5), ...)
```

## Arguments

- df:

  The data frame containing the accelerometry data.

- critAnglez:

  Numeric threshold representing the maximum allowed deviation of the Z
  angle within the bout. Default is 5.

- durBoutMin:

  The minimum continuous duration required to qualify as a SIB. Default
  is 5 minutes.

- ...:

  Additional arguments passed to `criterioSIB`.

## Value

A validated lubridate interval sequence of SIBs.
