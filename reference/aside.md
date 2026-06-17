# Multiple aside functions with base R pipe

Multiple aside functions with base R pipe

## Usage

``` r
aside(x, ...)
```

## Arguments

- x:

  An object

- ...:

  functions to run aside

## Examples

``` r
n_try <- 1
rnorm(200) |>
  matrix(ncol = 2) |>
  aside(
    print("Matrix prepared"),
    print(n_try)
  ) |>
  colSums()
#> [1] "Matrix prepared"
#> [1] 1
#> [1]  7.07892 11.48568
```
