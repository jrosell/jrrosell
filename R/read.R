#' Read character columns with clean names
#'
#' It's useful for reading the most common types of flat file data,
#' comma separated values and tab separated values.
#'
#' @rdname read_chr
#' @keywords read
#' @param file Either a path to a file, a connection, or literal data (either a single string or a raw vector).
#' @param delim Single character used to separate fields within a record.
#' @param locale The locale controls defaults that vary from place to place. The default locale is US-centric (like R), but you can use locale() to create your own locale that controls things like the default time zone, encoding, decimal mark, big mark, and day/month names.
#' @param ... Other parameters to readr::read_delim.
#'
#' @details The read_chr function works like \code{readr::read_delim}, except that
#' column sreturned would be characters and with clean names. It requires readr and janitor packages installed.
#'
#' @examples
#' es <- readr::locale("es", tz = "Europe/Madrid", decimal_mark = ",", grouping_mark = ".")
#' read_chr(readr::readr_example("mtcars.csv"), delim = ",", locale = es)
#'
#' @export
read_chr <- function(file, delim = ",", locale, ...) {
  if (!requireNamespace("readr", quietly = TRUE)) {
    return(NULL)
  }
  if (!requireNamespace("janitor", quietly = TRUE)) {
    return(NULL)
  }

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
#' @keywords read
#' @param url Full url including http or https protocol and the page path.
#' @param sleep Seconds to sleep after the request is done and before returning the result.
#'
#' @details The read_url function works uses rvest::read_html and purr::possibly
#' and it's fault tolearnt.
#'
#' @examples
#' read_url("https://www.google.cat/", sleep = 1)
#'
#' @export
read_url <- function(url, sleep = 0) {
  if (!requireNamespace("purrr", quietly = TRUE)) stop("purrr package is required")
  if (!requireNamespace("httr", quietly = TRUE)) stop("httr package is required")

  possibly_read_url <- purrr::possibly(httr::GET, quiet = TRUE) |>
    purrr::possibly(rawToChar, quiet = TRUE)

  result <- possibly_read_url(url)
  Sys.sleep(sleep)

  return(result)
}



#' Read a sheet from a xlsx file into a tibbles
#'
#' It's useful for reading a single sheets from a Excel/Openoffice file.
#'
#' @rdname read_xlsx
#' @keywords read
#' @param xlsxFile The name of the file.
#' @param ... Other parameters to openxls::read.xlsx function
#' @param sheet The name or index of the sheet (default 1)
#' @param startRow The number of the starting reading row (default 1)
#'
#' @details The write_xlsx it's a wroapper for \code{openxls::write.xlsx}.
#'
#' @examples
#' l <- list("IRIS" = iris, "MTCATS" = mtcars, matrix(runif(1000), ncol = 5))
#' tmp_file <- tempfile(fileext = ".xlsx")
#' write_xlsx(l, tmp_file, colWidths = c(NA, "auto", "auto"))
#' read_xlsx(tmp_file)
#' file.remove(tmp_file)
#'
#' @export
read_xlsx <- function(xlsxFile, ..., sheet = 1, startRow = 1) {
  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    return(NULL)
  }
  openxlsx::read.xlsx(
    xlsxFile = xlsxFile,
    sheet = sheet,
    startRow = startRow,
    ...
  )
}
