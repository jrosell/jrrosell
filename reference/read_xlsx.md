# Read a sheet from a xlsx file into a tibbles

It's useful for reading a single sheets from a Excel/Openoffice file.

## Usage

``` r
read_xlsx(xlsxFile, ..., sheet = 1, startRow = 1)
```

## Arguments

- xlsxFile:

  The name of the file.

- ...:

  Other parameters to openxls::read.xlsx function

- sheet:

  The name or index of the sheet (default 1)

- startRow:

  The number of the starting reading row (default 1)

## Details

The write_xlsx it's a wroapper for `openxls::write.xlsx`.

## Examples

``` r
l <- list("IRIS" = iris, "MTCARS" = mtcars)
tmp_file <- tempfile(fileext = ".xlsx")
write_xlsx(l, tmp_file)
df <- read_xlsx(tmp_file)
file.remove(tmp_file)
#> [1] TRUE
```
