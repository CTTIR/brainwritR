# Sheet assigned to a participant within a group

Sheet counts are frozen at session start. Modulo mapping permits several
participants to contribute to the same sheet in one round without
replacing one another's entries. A smaller arriving group can leave
sheets untouched.

## Usage

``` r
sheet_for(idx, n_sheets)
```

## Arguments

- idx:

  Positive integer within-group index.

- n_sheets:

  Positive integer number of sheets for the topic.

## Value

A numeric vector of one-based sheet indices.

## Examples

``` r
sheet_for(1:6, 5)
#> [1] 1 2 3 4 5 1
```
