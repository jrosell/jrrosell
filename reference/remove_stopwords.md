# Remove stopwords

This function processes character vectors and remove the specified stop
words or the stoop words of the langauge from the tm package

## Usage

``` r
remove_stopwords(text, stopwords = NULL, lang = "spanish")
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

## Value

A character vector without stopwords
