#' Write a list of tibbles to a xlsx file
#'
#' It's useful for saving multiple data to a multiple sheets of a single Excel/Openoffice/libreoffice file.
#'
#' @rdname write_xlsx
#' @keywords write
#' @param data A named list of tibbles
#' @param distfile The name of the destination file.
#' @param ... Other parameters to openxls::write.xlsx function
#'
#' @details The write_xlsx it's a wroapper for \code{openxls::write.xlsx}.
#'
#' @examples
#' l <- list("IRIS" = iris, "MTCATS" = mtcars, matrix(runif(1000), ncol = 5))
#' tmp_file <- tempfile(fileext = ".xlsx")
#' write_xlsx(l, tmp_file, colWidths = c(NA, "auto", "auto"))
#' file.remove(tmp_file)
#'
#' @export
write_xlsx <- function(data, distfile, ...) {
  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    return(NULL)
  }
  openxlsx::write.xlsx(
    x = data,
    file = distfile,
    ...
  )
}
