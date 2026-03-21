# Obtain the intervals formed by the best candidates every day to be considered as time and sleeping in bed

For each day of accelerometry recorded choose an interval as best
candidate to be considered as interval in bed, and inside of it, mark
another interval as time from first sleep to awake before going out of
bed

## Usage

``` r
intervalBiggerOfDay(intervalo, intervalo2 = NULL)
```

## Arguments

- intervalSib:

  dataframe of intervals considered as SIB

- distance1:

  distance allowed in SIB intervals to consider that the form part of
  the same sleep period and not different sleeping periods

- distance2:

  distance allowed in intervalQuiet to connect intervals of low
  activity, considered as taking part in the same low activity period.

## Value

A dataframe of intervals representing the intervals of being bed and
sleeping for every day of accelerometry data
