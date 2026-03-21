# Retrieve Information of Files in a Directory

Scans a directory for files matching a specific prefix and suffix, and
extracts metadata such as full path, size, modify time (mtime), and the
base variable name without the prefix/suffix.

## Usage

``` r
getInfoOfFiles(dir, prefix = "", suffix = "\\.RData$")
```

## Arguments

- dir:

  A string indicating the path to the directory to scan.

- prefix:

  A string for the required file prefix. If empty, it is ignored.

- suffix:

  A regex pattern for the required file suffix. Defaults to
  \code"\\.RData\$".

## Value

A tibble with columns: `file`, `path`, `RAW` (the clean name), `size`,
and `mtime`.
