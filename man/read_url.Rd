% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read.R
\name{read_url}
\alias{read_url}
\title{Read the html text of an url}
\usage{
read_url(url, sleep = 1, capacity = 1, realm = NULL)
}
\arguments{
\item{url}{Full URL to request}

\item{sleep}{Time (in seconds) to refill the bucket. Default: 1}

\item{capacity}{Max requests per refill period. Default: 1 (i.e., one request every \code{sleep} seconds)}

\item{realm}{Optional unique throttling scope. Defaults to domain of URL.}
}
\value{
HTML content as string or NULL on failure
}
\description{
Read the HTML text of a URL with rate-limiting
}
\details{
It's useful for getting the text of webpages in a single character vector.
}
\examples{
if (FALSE) read_url("https://www.google.cat/", sleep = 1)

}
\keyword{read}
