# Add hash for each row

It sorts the column names, it hash every row and add the column.

## Usage

``` r
add_row_hash(df, primary_keys)
```

## Arguments

- df:

  a data.frame

- primary_keys:

  the column anmes of the primary key

## Examples

``` r
df <- data.frame(
  id = c(1, 2, 3),
  name = c("AAAAA", "BBBB", "CCC")
)
add_row_hash(df, id)
#>   id  name                         row_hash
#> 1  1 AAAAA ba4e70c509c64f4dc8d06c162c8c053c
#> 2  2  BBBB 665a0897d0b2b95a72afbb1dfaa8f753
#> 3  3   CCC 62d9fe65ce0f1a9beae690e0f3251bab
```
