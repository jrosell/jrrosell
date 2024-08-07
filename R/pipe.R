#' Tee pipe that return the original value instead of the result
#'
#' Pipe a value forward into a functio or call expression and return the
#' original value instead of the result. This is useful when an expression
#' is used for its side-effect, say plotting or printing.
#'
#' @rdname tee
#' @keywords processing
#' @param x An object
#' @param expr An expresion
#'
#' @details The tee pipe works like \code{|>}, except the
#' return value is `x` itself, and not the result of `expr` call.
#'
#' @examples
#' rnorm(200) |>
#'     matrix(ncol = 2) |>
#'     tee(plot) |> # plot usually does not return anything.
#'     colSums()
#'
#' @section Thanks:
#'
#' I want to give credit to Michael Milton and Matthew Kay for the idea and the code.
#'
#' @source <https://mastodon.social/@multimeric@genomic.social/109555362766969210>
#' @seealso <https://github.com/jrosell/jrrosell/blob/main/R/pipe.R>
#' @export
tee <- function(x, expr) {
    expr = substitute(expr)
    eval(expr, list(x = x), parent.frame())
    x
}
