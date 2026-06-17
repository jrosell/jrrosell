# Plot bars for non double columns

Plot bars for non double columns

## Usage

``` r
plot_bars(df, ..., top_values = 50)
```

## Arguments

- df:

  a data.frame

- ...:

  optional parameters to geom_histogram

- top_values:

  fist most common values (default 50)

## Examples

``` r
plot_bars(data.frame(a = c("x", "y"), b = c("z", "z")))
```
