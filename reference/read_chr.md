# Read character columns with clean names

It's useful for reading the most common types of flat file data, comma
separated values and tab separated values.

## Usage

``` r
read_chr(
  file,
  delim = ",",
  locale = NULL,
  ...,
  date_names = "en",
  date_format = "%AD",
  time_format = "%AT",
  decimal_mark = ".",
  grouping_mark = "",
  tz = "CET",
  encoding = "UTF-8",
  asciify = FALSE
)
```

## Arguments

- file:

  Either a path to a file, a connection, or literal data (either a
  single string or a raw vector).

- delim:

  Single character used to separate fields within a record.

- locale:

  The locale controls defaults that vary from place to place. The
  default locale is US-centric (like R), but you can use locale() to
  create your own locale that controls things like the default time
  zone, encoding, decimal mark, big mark, and day/month names.

- ...:

  Other parameters to readr::read_delim.

- date_names:

  "en" from readr::locale

- date_format:

  "%AD" from readr::locale

- time_format:

  "%AT" from readr::locale

- decimal_mark:

  "." from readr::locale

- grouping_mark:

  "" from readr::locale

- tz:

  "CET"

- encoding:

  "UTF-8"

- asciify:

  FALSE

## Details

The read_chr function works like
[`readr::read_delim`](https://readr.tidyverse.org/reference/read_delim.html),
except that column sreturned would be characters and with clean names.
It requires readr and janitor packages installed.

## Examples

``` r
read_chr(readr::readr_example("mtcars.csv"), delim = ",")
#> # A tibble: 32 × 11
#>    mpg   cyl   disp  hp    drat  wt    qsec  vs    am    gear  carb 
#>    <chr> <chr> <chr> <chr> <chr> <chr> <chr> <chr> <chr> <chr> <chr>
#>  1 21    6     160   110   3.9   2.62  16.46 0     1     4     4    
#>  2 21    6     160   110   3.9   2.875 17.02 0     1     4     4    
#>  3 22.8  4     108   93    3.85  2.32  18.61 1     1     4     1    
#>  4 21.4  6     258   110   3.08  3.215 19.44 1     0     3     1    
#>  5 18.7  8     360   175   3.15  3.44  17.02 0     0     3     2    
#>  6 18.1  6     225   105   2.76  3.46  20.22 1     0     3     1    
#>  7 14.3  8     360   245   3.21  3.57  15.84 0     0     3     4    
#>  8 24.4  4     146.7 62    3.69  3.19  20    1     0     4     2    
#>  9 22.8  4     140.8 95    3.92  3.15  22.9  1     0     4     2    
#> 10 19.2  6     167.6 123   3.92  3.44  18.3  1     0     4     4    
#> # ℹ 22 more rows
```
