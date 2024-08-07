% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/types.R
\name{as.bitstring}
\alias{as.bitstring}
\title{Data type utilities}
\source{
https://youtu.be/J4DnzjIFj8w
}
\usage{
as.bitstring(x)
}
\arguments{
\item{x}{A numeric vetor.}
}
\description{
Get the bit representation of a double number
}
\details{
Get the bit representation of a double number
Using rev() ensures that the bit order is correct, and the binary representation aligns with the usual convention of having the MSB first and the LSB last.
This is because numToBits() returns the bits in the reverse order, and without rev(), we end up with the LSB first and the MSB last.
}
\examples{
0.1 + 0.2 == 0.3
as.bitstring(0.1 + 0.2)
as.bitstring(0.3)

}
\seealso{
https://github.com/jrosell/jrrosell/blob/main/R/types.R
}
\keyword{types}
