#' Data type utilities
#'
#' Get the bit representation of a double number
#'
#' @rdname as.bitstring
#' @param x A numeric vetor.
#'
#' @details Get the bit representation of a double number
#'
#' @examples
#' 0.1 + 0.2 == 0.3
#' as.bitstring(0.1 + 0.2)
#' as.bitstring(0.3)
#'
#' @source https://youtu.be/J4DnzjIFj8w
#' @seealso https://github.com/jrosell/jrrosell/blob/main/R/types.R
#' @export
as.bitstring <- function(x) {
    bits <- numToBits(as.double(x))
    bitstring <- paste(rev(bits), collapse = "")
    return(bitstring)
}
