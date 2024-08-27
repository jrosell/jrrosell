#' Data type utilities
#'
#' Get the bit representation of a double number
#'
#' @rdname as.bitstring
#' @keywords types
#' @param x A numeric vetor.
#'
#' @details Get the bit representation of a double number
#' Using rev() ensures that the bit order is correct, and the binary representation aligns with the usual convention of having the MSB first and the LSB last.
#' This is because numToBits() returns the bits in the reverse order, and without rev(), we end up with the LSB first and the MSB last.
#'
#' @examples
#' 0.1 + 0.2 == 0.3
#' as.bitstring(0.1 + 0.2)
#' as.bitstring(0.3)
#'
#' @source https://youtu.be/J4DnzjIFj8w
#' @export
as.bitstring <- function(x) {
  bits <- numToBits(as.double(x))
  bitstring <- paste(rev(bits), collapse = "")
  return(bitstring)
}
