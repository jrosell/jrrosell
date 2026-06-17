# Count a variable or variables sorted

It returns the ordered counts of the variable in the data.frame.

## Usage

``` r
count_sorted(df, ...)
```

## Arguments

- df:

  a data.frame

- ...:

  the variables to use and other arguments to count

## Examples

``` r
data.frame(a = c("x", "y", "x"), b = c("z", "z", "n")) |>
  count_sorted(a)
#>   a n
#> 1 x 2
#> 2 y 1
```
