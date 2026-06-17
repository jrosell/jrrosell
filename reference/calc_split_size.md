# Calculate split size

From binary classification problems, with the desired std_err it returns
the minimal assesment/validation set size.

## Usage

``` r
calc_split_size(
  std_err = NULL,
  confidence_interval = 0.95,
  margin_error = 0.02
)
```

## Source

<https://stats.stackexchange.com/a/304996/7387>

## Arguments

- std_err:

  The desired std_err numeric (default NULL)

- confidence_interval:

  (default 0.95)

- margin_error:

  (default 0.02)

## Details

The calc_validation_size function returns the minimal validation size
for expected probabilities and desired error. s

## Examples

``` r
calc_split_size()
#> Info: 0.0102042691384931 std_err is used.
#> [1] 2400.912
calc_split_size(confidence_interval = 0.95, margin_error = 0.02)
#> Info: 0.0102042691384931 std_err is used.
#> [1] 2400.912
calc_split_size(std_err = 0.02)
#> [1] 625
```
