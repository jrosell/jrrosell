# Data type utilities

Get the bit representation of a double number

## Usage

``` r
as.bitstring(x)
```

## Source

https://youtu.be/J4DnzjIFj8w

## Arguments

- x:

  A numeric vetor.

## Details

Get the bit representation of a double number Using rev() ensures that
the bit order is correct, and the binary representation aligns with the
usual convention of having the MSB first and the LSB last. This is
because numToBits() returns the bits in the reverse order, and without
rev(), we end up with the LSB first and the MSB last.

## Examples

``` r
0.1 + 0.2 == 0.3
#> [1] FALSE
as.bitstring(0.1 + 0.2)
#> [1] "00000101010101010101000100000101000001010000010100000101000001010000010100000101000001010000010100000101000001010000010100010000"
as.bitstring(0.3)
#> [1] "00000101010101010101000100000101000001010000010100000101000001010000010100000101000001010000010100000101000001010000010100000101"
```
