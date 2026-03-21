# Summarises Sustained Inactivity Bouts (SIB) according to the criteria by Vincent van Hees Critera: Identify 5-min-bouts with \< 5 degrees change in angle.

Summarises Sustained Inactivity Bouts (SIB) according to the criteria by
Vincent van Hees Critera: Identify 5-min-bouts with \< 5 degrees change
in angle.

## Usage

``` r
SIB(df, critAnglez = 5, durBoutMin = dminutes(5), ...)
```

## Arguments

- df:

  Dataframe of epochs with two columns: datetime and XXXXXX.

## Value

a list with a summary of the periods matching the criteria.
