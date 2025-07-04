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

#' @param date_names "en" from readr::locale
#' @param date_format "%AD" from readr::locale
#' @param time_format "%AT" from readr::locale
#' @param decimal_mark "." from readr::locale
#' @param grouping_mark "" from readr::locale
#' @param tz "CET"
#' @param encoding "UTF-8"
#' @param asciify FALSE
#'
#' @details The read_chr function works like \code{readr::read_delim}, except that
#' column sreturned would be characters and with clean names. It requires readr and janitor packages installed.
#'
#' @examples
#' read_chr(readr::readr_example("mtcars.csv"), delim = ",")
#'
#' @export
read_chr <- function(
    file,
    delim = ",",
    locale = NULL,
    ...,
    date_names = "en",
    date_format = "%AD",
    time_format = "%AT",
    decimal_mark = ".",
    grouping_mark = "",
    tz = "CET",
    encoding = "UTF-8",
    asciify = FALSE) {
  if (!requireNamespace("readr", quietly = TRUE)) {
    return(NULL)
  }
  if (!requireNamespace("janitor", quietly = TRUE)) {
    return(NULL)
  }
  if (is.null(locale)) {
    locale <- readr::locale(
      date_names = date_names,
      date_format = date_format,
      time_format = time_format,
      decimal_mark = decimal_mark,
      grouping_mark = grouping_mark,
      tz = tz,
      encoding = encoding,
      asciify = FALSE
    )
  }
  readr::read_delim(
    file,
    delim,
    col_types = readr::cols(.default = "c"),
    name_repair = janitor::make_clean_names,
    skip_empty_rows = FALSE,
    locale = locale,
    ...
  )
}

#' Read the html text of an url
#'


#' Read the HTML text of a URL with rate-limiting
#'
#' It's useful for getting the text of webpages in a single character vector.
#'
#' @rdname read_url
#' @keywords read
#' @param url Full URL to request
#' @param sleep Time (in seconds) to refill the bucket. Default: 1
#' @param capacity Max requests per refill period. Default: 1 (i.e., one request every `sleep` seconds)
#' @param realm Optional unique throttling scope. Defaults to domain of URL.
#'
#' @return HTML content as string or NULL on failure
#'
#' @examples
#' if (FALSE) read_url("https://www.google.cat/", sleep = 1)
#'
#' @export
read_url <- function(url, sleep = 1, capacity = 1, realm = NULL) {
  if (!requireNamespace("httr2", quietly = TRUE)) stop("httr2 package required")
  if (!requireNamespace("purrr", quietly = TRUE)) stop("purrr package required")

  possibly_read <- purrr::possibly(function(url) {
    req <- httr2::request(url) |>
      httr2::req_throttle(capacity = capacity, fill_time_s = sleep, realm = realm)

    resp <- httr2::req_perform(req)
    httr2::resp_body_string(resp)
  }, otherwise = NULL, quiet = TRUE)

  possibly_read(url)
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
#' l <- list("IRIS" = iris, "MTCARS" = mtcars, matrix(runif(1000), ncol = 5))
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

#' @noRd
is_response <- function(x) {
  inherits(x, "httr2_response")
}

#' @noRd
check_response <- function(resp, arg = rlang::caller_arg(resp), call = rlang::caller_env()) {
  if (!missing(resp) && is_response(resp)) {
    return(invisible(NULL))
  }

  message <- sprintf(
    "%s must be %s.",
    arg,
    "an HTTP response object"
  )
  rlang::abort(message, call = call, arg = arg)
}

#' @noRd
request_test <- function(template = "/get", ...) {
  req <- httr2::request(httr2::example_url())
  req <- httr2::req_template(req, template, ..., .env = rlang::caller_env())
  req
}

#' Extract body from httr2 response using yyjsonr
#'
#' @rdname resp_body_yyjson
#' @keywords read
#' @param resp A httr2::response object, created by httr2::req_perform().
#' @param check_type Should the type actually be checked? Provided as a
#'   convenience for when using this function inside `resp_body_*` helpers.
#' @param simplifyVector Should JSON arrays containing only primitives (i.e.
#'   booleans, numbers, and strings) be caused to atomic vectors?
#' @param ... Other parameters
#'
#' @export
resp_body_yyjson <- function(
    resp,
    check_type = TRUE,
    simplifyVector = FALSE,
    ...) {
  check_response(resp)
  rlang::check_installed("yyjsonr")
  httr2::resp_check_content_type(
    resp,
    valid_types = "application/json",
    valid_suffix = "json",
    check_type = check_type
  )
  text <- httr2::resp_body_string(resp, "UTF-8")
  yyjsonr::read_json_str(
    text,
    length1_array_asis = !simplifyVector,
    ...
  )
}
