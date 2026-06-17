# Normalize text

This function processes a given text string by converting it to
lowercase, removing numbers, non-alphanumeric characters, extra
whitespace. It also transliterates text to ASCII, splits words, and
reconstructs a clean text string suitable for analysis.

## Usage

``` r
normalize_text(text, remove_digits = TRUE, remove_accents = TRUE)
```

## Arguments

- text:

  A character vector or object that can be coerced to a character
  string. Represents the input text to be cleaned.

- remove_digits:

  = TRUE

- remove_accents:

  = TRUE

## Value

A normalized character vector
