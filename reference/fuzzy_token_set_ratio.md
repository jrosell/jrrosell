# Fuzzy Token Set Ratio

This function computes a fuzzy similarity score between two strings
based on the token set ratio methodology. It considers the intersection
and differences between tokenized word sets from the input strings, and
calculates a similarity score normalized by string lengths.

## Usage

``` r
fuzzy_token_set_ratio(s1, s2, score_cutoff = 0)
```

## Arguments

- s1:

  A character string. The first string to compare.

- s2:

  A character string. The second string to compare.

- score_cutoff:

  A numeric value (default is `0`) specifying the minimum similarity
  score threshold. Scores below this threshold may trigger early exits
  in the computation.

## Value

A numeric similarity score between `0` and `100`, representing the
degree of similarity between the two input strings.

## Details

This function performs the following steps:

- Tokenizes the input strings.

- Identifies intersecting and differing tokens between the two tokenized
  sets.

- Computes the longest common subsequence (LCS) distance for differing
  tokens and normalizes it.

- Calculates similarity ratios for intersecting tokens combined with
  differing token sets.

- Returns the maximum of the normalized LCS distance and the two
  intersecting token ratios.

The function short-circuits to return `100` if one token set is a subset
of the other. If either input string is empty, the function returns `0`.

## Examples

``` r
# Example usage:
fuzzy_token_set_ratio("fuzzy was a bear", "fuzzy was a dog", score_cutoff = 80)
#> [1] 84.61538
fuzzy_token_set_ratio("hello world", "world hello")
#> [1] 100

```
