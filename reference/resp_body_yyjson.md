# Extract body from httr2 response using yyjsonr

Extract body from httr2 response using yyjsonr

## Usage

``` r
resp_body_yyjson(resp, check_type = TRUE, simplifyVector = FALSE, ...)
```

## Arguments

- resp:

  A httr2::response object, created by httr2::req_perform().

- check_type:

  Should the type actually be checked? Provided as a convenience for
  when using this function inside `resp_body_*` helpers.

- simplifyVector:

  Should JSON arrays containing only primitives (i.e. booleans, numbers,
  and strings) be caused to atomic vectors?

- ...:

  Other parameters
