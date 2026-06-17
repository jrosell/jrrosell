# Plot histograms for double columns

Plot histograms for double columns

## Usage

``` r
plot_histograms(df, ...)
```

## Arguments

- df:

  a data.frame

- ...:

  optional parameters to geom_histogram

## Examples

``` r
plot_histograms(data.frame(a = c(1, 2), b = c(1, 3)))
#> `stat_bin()` using `bins = 30`. Pick better value `binwidth`.
```
