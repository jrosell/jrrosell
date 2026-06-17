# Detect cores that could be used

Select cores in max/min of the available cores.

## Usage

``` r
detect_cores(max = 10, min = 2)
```

## Arguments

- max:

  An integer with the max desired cores (default 10)

- min:

  An integer with the min desired cores (default 2)

## Details

The detect_cores function uses parallelly package. It returns the
desired max cores if available or it fails if not min cores are
available (excluding parallelly.availableCores.omit reserved cores or 1
if not defined).

## Examples

``` r
cores <- detect_cores(max = 5, min = 1)
print(cores)
#> [1] 3
```
