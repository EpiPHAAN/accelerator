# Convert a data frame with a column called .criterio to a data frame of intervals of bouts of that criteria

data frame of intervals of Bouts meeting certain criteria,

## Usage

``` r
criterio2Interval(df, pctBouts = 1, durBoutMin = dseconds(5), units = "mins")
```

## Arguments

- df:

  data frame with columns .criterio

- durBoutMin:

  minimum amount of time that the condition muest be met to be
  aconsidered a Bout

- units:

  Units of time to show certain summaries. Une of
  c("secs","mins","hours","days")

- pctBout:

  represents fraction of time that the .criterio must be TRUE

- durEpoch:

  amount of time that represents each row of the dataframe (duration of
  a epoch usually)

## Value

a list with data frame of intervals and certain summaries.
