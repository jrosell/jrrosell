# Glimpse multiple datasets

Glimpse multiple datasets

## Usage

``` r
glimpses(...)
```

## Arguments

- ...:

  Multiple data.frame

## Examples

``` r
df1 <- data.frame(a = c(1, 2))
df2 <- data.frame(b = c(3, 4))
glimpses(df1, df2)
#> df1 2 rows and 1 columns with a 2 distinct values. 
#> df2 2 rows and 1 columns with b 2 distinct values. 
```
