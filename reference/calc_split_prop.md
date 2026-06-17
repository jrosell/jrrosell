# Calculate split proportion

From a data frame, it returns the minimal split proportion for
validation.

## Usage

``` r
calc_split_prop(df, k = 10)
```

## Source

<https://stats.stackexchange.com/a/305063/7387>

## Arguments

- df:

  A data frame

- k:

  number of desired folds (default 10)

## Details

The calc_validation_size function returns the optimal split proportion
according to the number of rows for your validation set.

## Examples

``` r
calc_split_prop(data.frame(row = 1:891))
#> Info: Minimal validation split of 21/891 rows.
#> Info: Resamples of 7/87 rows for 10 of folds.
#> [1] 0.02368897
```
