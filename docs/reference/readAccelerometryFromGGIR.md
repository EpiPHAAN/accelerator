# Read Accelerometry Data from GGIR Outuput

Creates and returns a factory function that loads and standardizes GGIR
time-series (\`metashort\` and \`metalong\`) data into an appropriate
tibble. Applies timezone adjustments and identifies Non-Wear time.

## Usage

``` r
readAccelerometryFromGGIR(
  path_ggir_ts = "",
  base = "",
  RAW = "",
  start = NA,
  end = NA,
  tz = "Europe/Madrid",
  ...
)
```

## Arguments

- path_ggir_ts:

  The explicit absolute file path to the RData file.

- base:

  Directory base string to construct the path if `path_ggir_ts` is not
  provided.

- RAW:

  Name of the raw data block to construct the path if `path_ggir_ts` is
  not provided.

- start:

  Optional start constraint (currently ignored by internal logic but
  kept for interface compatibility).

- end:

  Optional end constraint.

- tz:

  Timezone string. Default is `"Europe/Madrid"`.

- ...:

  Additional arguments.

## Value

A function without arguments that, when executed, loads the specified
GGIR environment and returns the formatted data frame.
