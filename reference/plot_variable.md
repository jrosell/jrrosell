# Plot a variable

It returns a bar or a histogram of the variable

## Usage

``` r
plot_variable(df, variable, ..., type = "numeric")
```

## Arguments

- df:

  a data.frame

- variable:

  the variable to use.

- ...:

  params passed to geom\_\*

- type:

  numeric (default) or nominal.

## Examples

``` r
data.frame(a = c("x", "y", "y"), b = c("z", "z", "x")) |> plot_variable(a)
```
