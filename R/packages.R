#' Github version of the package
#'
#' Get the version from the DESCRIPTION file of the master branch in the github repo
#'
#' @rdname package_github_version
#' @keywords packages
#' @param x a single repo/package to check Ex: package_github_version("tidyverse/dplyr")
#' @param file_lines (default = NULL, internal)
#' @examples
#' if (FALSE) {
#'   package_github_version("jrosell/jrrosell")
#' }
#' @export
package_github_version <- function(x, file_lines = NULL) {
  i <- grepl("Version: ", file_lines)
  if (length(i) == 0) {
    file_lines <- package_github_read(x)
  }
  version <- substring(file_lines[grepl("Version: ", file_lines)], 10)
  return(version)
}

#' Github name of the package
#'
#' Get the name of the package from the DESCRIPTION file of the master branch in the github repo
#'
#' @rdname package_github_name
#' @keywords packages
#' @param x a single repo/package to check Ex: package_github_name("tidyverse/dplyr")
#' @param file_lines (default = NULL, internal)
#' @examples
#' if (FALSE) {
#'   package_github_name("jrosell/jrrosell")
#' }
#' @export
package_github_name <- function(x, file_lines = NULL) {
  i <- grepl("Package: ", file_lines)
  if (length(i) == 0) {
    file_lines <- package_github_read(x)
  }
  name <- substring(file_lines[grepl("Package: ", file_lines)], 10)
  return(name)
}

#' Check if the last github version is installed
#'
#' Check if the last main github version is installed.
#'
#' @rdname check_installed_github
#' @keywords packages
#' @param repo a github repo/package. Ex: check_installed_github("tidyverse/dplyr")
#' @examples
#' if (FALSE) {
#'   check_installed_github("jrosell/jrrosell")
#' }
#' @export
check_installed_github <- function(repo) {
  # options("rlib_restart_package_not_found" = TRUE)
  file_lines <- package_github_read(repo)
  name <- package_github_name(repo, file_lines = file_lines)
  version <- jrrosell::package_github_version(repo, file_lines = file_lines)
  pkg <- paste0(name, " (>= ", version, ")")
  rlang::check_installed(pkg, action = \(pkg, ...) pak::pak(repo))
}


#' @noRd
#' @keywords internal
package_github_read <- function(x) {
  glue::glue("https://raw.githubusercontent.com/{x}/master/DESCRIPTION") |>
    readLines()
}
