# Prepare docs for Analysis

Prepare docs for Analysis

## Usage

``` r
prepare_docs(df, ...)
```

## Arguments

- df:

  data frame with and id and text columns.

- ...:

  paramters passed to `"prepare_tokens"`

## Value

A df with a list of tokens and character vector prepared_text columns
for documents at column id and text at column "text"

## Examples

``` r
# Example usage:
prepare_docs(data.frame(id = 1, text = "¡Hola! Esto es una prueba 123."))
#>   id                           text                      tokens
#> 1  1 ¡Hola! Esto es una prueba 123. hola, esto, es, una, prueba
#>             prepared_text
#> 1 hola esto es una prueba
```
