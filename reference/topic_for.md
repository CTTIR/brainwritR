# Topic assigned to a group in a round

Over `n_groups` consecutive rounds, each group visits every topic
exactly once. In each round the groups cover all topics. Arguments
recycle using ordinary R arithmetic; group and round indices are
one-based.

## Usage

``` r
topic_for(grp, round, n_groups)
```

## Arguments

- grp:

  Positive integer group index, at most `n_groups`.

- round:

  Positive integer round index.

- n_groups:

  Integer number of groups (2 to 6).

## Value

A numeric vector of one-based topic indices.

## Examples

``` r
topic_for(1:3, 2, 3)
#> [1] 2 3 1
topic_for(1, 1:3, 3)
#> [1] 1 2 3
```
