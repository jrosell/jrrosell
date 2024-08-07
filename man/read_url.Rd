% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read.R
\name{read_url}
\alias{read_url}
\title{Read the html text of an url}
\usage{
read_url(url, sleep = 0)
}
\arguments{
\item{url}{Full url including http or https protocol and the page path.}

\item{sleep}{Seconds to sleep after the request is done and before returning the result.}
}
\description{
It's useful for getting the text for webpages in a single character vector.
}
\details{
The read_url function works uses rvest::read_html and purr::possibly
and it's fault tolearnt.
}
\examples{
read_url("https://www.google.cat/", sleep = 1)

}
\seealso{
\url{https://github.com/jrosell/jrrosell/blob/main/R/read.R}
}
\keyword{read}
