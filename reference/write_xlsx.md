# Write a list of tibbles to a xlsx file

It's useful for saving multiple data to a multiple sheets of a single
Excel/Openoffice/libreoffice file.

## Usage

``` r
write_xlsx(data, distfile, ...)
```

## Arguments

- data:

  A named list of tibbles

- distfile:

  The name of the destination file.

- ...:

  Other parameters to openxls::write.xlsx function

## Details

The write_xlsx it's a wroapper for `openxls::write.xlsx`.
