# Indicates which rows meets certain limits for ENMO

Generates a vector that indicates for each row of a dataframe (usually
epoch or BIN file) if that row verifies the condition
limInf\<=df\[\["ENMO"\]\] \<=limSup

## Usage

``` r
criterioENMO(df, limInf = 0, limSup = Inf, useNW = TRUE)
```

## Arguments

- df:

  data frame with columns ENMO and eventually .criterioNW (that
  represents NonWear time as TRUE/FALSE)

- limInf:

  inferior limit for ENMO

- limSup:

  superior limit for ENMO

## Value

a boolean vector (TRUE/FAlSE) indicating if the condition is met.
