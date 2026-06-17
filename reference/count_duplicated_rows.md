# Count the number of duplicated rows

Count the number of duplicated rows

## Usage

``` r
count_duplicated_rows(df)
```

## Arguments

- df:

  a data.frame

## Examples

``` r
count_duplicated_rows(data.frame(a = c(1, 2, 3), b = c(3, 4, 5)))
#> [1] 0
count_duplicated_rows(data.frame(a = c(1, 2, 3), b = c(1, 4, 5)))
#> [1] 0
```
