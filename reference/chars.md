# Create a vector of characters from a string

Create a vector of characters from a string

## Usage

``` r
chars(x, ...)
```

## Arguments

- x:

  a vector of characters of length 1.

- ...:

  unused

## Value

a vector of characters

## Details

`chars` expects a single string as input. To create a list of these,
consider `lapply(strings, chars)`.

## See also

<https://github.com/jonocarroll/charcuterie>

## Examples

``` r
chars("hola")
#> [1] "h" "o" "l" "a"
```
