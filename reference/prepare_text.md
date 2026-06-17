# Prepare Text for Analysis

This function processes a given text string by converting it to
lowercase, removing numbers, non-alphanumeric characters, extra
whitespace, and stopwords based on a specified language. It also
transliterates text to ASCII, splits words, and reconstructs a clean
text string suitable for analysis.

## Usage

``` r
prepare_text(...)
```

## Arguments

- ...:

  paramters passed to `"prepare_tokens"`

## Value

A cleaned character string, with stopwords removed and text formatted
for analysis.

## Examples

``` r
# Example usage:
prepare_text("¡Hola! Esto es una prueba 123.")
#> [1] "hola prueba"
```
