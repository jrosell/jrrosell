#' Read character columns with clean names
#'
#' It's useful for reading the most common types of flat file data,
#' comma separated values and tab separated values.
#'
#' @rdname read_chr
#' @param file Either a path to a file, a connection, or literal data (either a single string or a raw vector).
#' @param delim Single character used to separate fields within a record.
#' @param locale The locale controls defaults that vary from place to place. The default locale is US-centric (like R), but you can use locale() to create your own locale that controls things like the default time zone, encoding, decimal mark, big mark, and day/month names.
#' @param ... Other parameters to readr::read_delim.
#'
#' @details The read_chr function works like \code{readr::read_delim}, except that
#' column sreturned would be characters and with clean names. It requires readr and janitor packages installed.
#'
#' @examples
#' es <- readr::locale("es", tz="Europe/Madrid", decimal_mark = ",", grouping_mark = ".")
#' read_chr(readr::readr_example("mtcars.csv"), delim = ",", locale = es)
#'
#' @seealso <https://github.com/jrosell/jrrosell/blob/main/R/read.R>
#' @export
read_chr <- function(file, delim = ",", locale, ...) {
    if(!requireNamespace("readr", quietly = TRUE)) return(NULL)
    if(!requireNamespace("janitor", quietly = TRUE)) return(NULL)

    readr::read_delim(
        file,
        delim,
        col_types = readr::cols(.default = "c"),
        name_repair = ~ janitor::make_clean_names(.),
        skip_empty_rows = FALSE,
        locale = locale,
        ...
    )
}

#' Read the html text of an url
#'
#' It's useful for getting the text for webpages in a single character vector.
#'
#' @rdname read_url
#' @param url Full url including http or https protocol and the page path.
#' @param sleep Seconds to sleep after the request is done and before returning the result.
#'
#' @details The read_url function works uses rvest::read_html and purr::possibly
#' and it's fault tolearnt.
#'
#' @examples
#' read_url("https://www.google.es/search?q=jrosell", sleep = 1)
#'
#' @seealso <https://github.com/jrosell/jrrosell/blob/main/R/read.R>
#' @export
read_url <- function(url, sleep = 0) {
    if(!requireNamespace("purrr", quietly = TRUE)) stop("purrr package is required")
    if(!requireNamespace("httr", quietly = TRUE)) stop("httr package is required")

    possibly_read_url <- purrr::possibly(httr::GET, '') |>
        purrr::possibly(rawToChar, '')

    result <- possibly_read_url(url)
    Sys.sleep(sleep)

    return(result)
}

