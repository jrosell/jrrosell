# Prepare tokens from text for Analysis

This function processes a given text string by converting it to
lowercase, removing numbers, non-alphanumeric characters, extra
whitespace, and stopwords based on a specified language. It also
transliterates text to ASCII, splits words, and reconstructs a clean
text string suitable for analysis.

## Usage

``` r
prepare_tokens(
  text,
  stopwords = NULL,
  lang = "spanish",
  sep = "\\s+",
  remove_digits = TRUE,
  remove_accents = TRUE,
  lemmatize = c("none", "udpipe", "spacyr"),
  model_dir = getwd()
)
```

## Arguments

- text:

  A character vector or object that can be coerced to a character
  string. Represents the input text to be cleaned.

- stopwords:

  A character vector specifying stopwords removal. Defaults tm:stopwords
  package.

- lang:

  defaults to `"spanish"`

- sep:

  separator for spliting defaults to `"\\s+"`

- remove_digits:

  = TRUE

- remove_accents:

  = TRUE

- lemmatize:

  = c("none", "udpipe", "spacyr") defaults to `"none"`

- model_dir:

  defaults to getwd()

## Value

A cleaned character vector, with stopwords removed and text formatted
for analysis and can be lemmatized optionally and then returns a
character vector of lemmas.
