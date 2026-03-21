# Connect intervals that are close enough Generates a smaller dataframe of connected intervals using a data frame of given intervals and a distance that allows tow of them to be connected

Connect intervals that are close enough Generates a smaller dataframe of
connected intervals using a data frame of given intervals and a distance
that allows tow of them to be connected

## Usage

``` r
connectOverDistanceV2(
  interval,
  distanciaNoche = dhours(4),
  distanciaDia = dminutes(0),
  earlyMorning = c(1, 9),
  interruption = NULL
)
```

## Arguments

- interval:

  Initial dataframe of intervals to be connected

- distanciaNoche:

  distance between ttwo intervals to allo the connection of both in just
  one.

- distanciaDia:

  distance between ttwo intervals to allo the connection of both in just
  one.

## Value

A dataframe of intervals, having less or equal rows tan the original
