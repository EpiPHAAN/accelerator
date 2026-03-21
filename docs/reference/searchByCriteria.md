# Search Bouts by Dynamic Criteria

Dynamically applies a specific criterion string against a dataframe and
subsequently groups consecutive positively evaluated epochs into
distinct intervals (bouts) based on minimum percentage and duration
thresholds.

## Usage

``` r
searchByCriteria(
  df,
  EXPRESSION,
  pctBouts = 0.8,
  durBoutMin = dminutes(1),
  runmean = FALSE,
  useNW = TRUE
)
```

## Arguments

- df:

  The data frame of timeline/epoch measurements.

- EXPRESSION:

  A logical expression string (e.g., \`"ENMO \> 0.05"\`) to be
  dynamically parsed.

- pctBouts:

  The required fraction (percentage from 0 to 1) of time within the
  window that the criteria must hold. Default is 0.8.

- durBoutMin:

  The required minimum continuous rolling duration for the bout to be
  confirmed. Default is 1 minute.

- runmean:

  Logical. If TRUE, applies a running mean smoothing filter globally to
  referenced fields before searching. Default is FALSE.

- useNW:

  Logical. If TRUE, avoids returning bouts on detected NonWear time.
  Default is TRUE.

## Value

A list/vector of validated interval bouts (start and end times).
