# Request the maximum safe number of cores

When parallizing within resamples, required memory can crash the system.

## Usage

``` r
request_max_safe_cores_from_rss(
  estimated_max_rss,
  memory_usage = 0.5,
  verbose = TRUE
)
```

## Arguments

- estimated_max_rss:

  butes of maximum rss it will used (You can get it from syrup package)

- memory_usage:

  the proportion of the system memory that will be used (0.8)

- verbose:

  to debug (TRUE)

## Details

The detect_cores function uses parallelly package. It returns the
desired max cores if available or it fails if not min cores are
available (excluding system reserved cores).
