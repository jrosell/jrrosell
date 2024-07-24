#' Github version of the package
#'
#' Get the version from the DESCRIPTION file of the master branch in the github package
#'
#' @rdname package_github_version
#' @keywords processing
#' @param x a single package
#' 
#' @seealso https://github.com/jrosell/jrrosell/blob/main/R/packages.R
#' @export
package_github_version <- function(x) {
  url <- glue::glue("https://raw.githubusercontent.com/{x}/master/DESCRIPTION")
  lines <- readLines(url)
  version <- substring(lines[grepl("Version: ", lines)], 10)
  return(version)
}
