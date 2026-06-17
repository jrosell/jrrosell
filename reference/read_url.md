# Read the html text of an url

Read the HTML text of a URL with rate-limiting

## Usage

``` r
read_url(url, sleep = 1, capacity = 1, realm = NULL)
```

## Arguments

- url:

  Full URL to request

- sleep:

  Time (in seconds) to refill the bucket. Default: 1

- capacity:

  Max requests per refill period. Default: 1 (i.e., one request every
  `sleep` seconds)

- realm:

  Optional unique throttling scope. Defaults to domain of URL.

## Value

HTML content as string or NULL on failure

## Details

It's useful for getting the text of webpages in a single character
vector.

## Examples

``` r
if (FALSE) read_url("https://www.google.cat/", sleep = 1)
```
