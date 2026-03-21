# Indicates which rows meets the criteria for be considered part of a SIB (Sustained Inactivity Bouts)

Generates a vector that indicates for each row of a dataframe (usually
epoch or BIN file) if that row verifies the condition to be considered a
SIB, i.e Anglez variying a few degrees for a certain amount of time
(durBoutMin, and ENMO lower than a low value)

## Usage

``` r
criterioSIB(df, critAnglez = 5, limSup = 30/1000, durBoutMin = dminutes(5))
```

## Arguments

- df:

  data frame with columns ANGLEZ, ENMO and eventually .criterioNW (that
  represents NonWear time as TRUE/FALSE)

- critAnglez:

  represents maximum of deviation (in both directios) of angle Z that it
  is allowed during a SIB

- limSup:

  superior limit for ENMO

- durBoutMin:

  minimum amount of time that the conditions must be met to beconsidered
  a SIB period

## Value

a boolean vector (TRUE/FAlSE) indicating if the condition of belonging
to a SIB is met.
