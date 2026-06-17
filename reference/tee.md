# Tee pipe that return the original value instead of the result

Pipe a value forward into a functio or call expression and return the
original value instead of the result. This is useful when an expression
is used for its side-effect, say plotting or printing.

## Usage

``` r
tee(x, expr)
```

## Source

<https://mastodon.social/@multimeric@genomic.social/109555362766969210>

## Arguments

- x:

  An object

- expr:

  An expresion

## Details

The tee pipe works like `|>`, except the return value is `x` itself, and
not the result of `expr` call.

## Thanks

I want to give credit to Michael Milton and Matthew Kay for the idea and
the code.
