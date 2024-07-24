#' Github version of the package
#'
#' Get the version from the DESCRIPTION file of the master branch in the github package
#'
#' @rdname package_github_version
#' @keywords packages
#' @param x a single repo/package to check Ex: package_github_version("tidyverse/dplyr")
#' 
#' @seealso https://github.com/jrosell/jrrosell/blob/main/R/packages.R
#' @export
package_github_version <- function(x) {
  url <- glue::glue("https://raw.githubusercontent.com/{x}/master/DESCRIPTION")
  lines <- readLines(url)
  version <- substring(lines[grepl("Version: ", lines)], 10)
  return(version)
}

#' Check if the last github version is installed
#'
#' Get the version from the DESCRIPTION file of the master branch in the github package.
#'
#' @rdname check_installed_gihub
#' @keywords packages
#' @param repo a github repo/package. Ex: check_installed_gihub("tidyverse/dplyr")
#' 
#' @seealso https://github.com/jrosell/jrrosell/blob/main/R/packages.R
#' @export
check_installed_gihub <- function(repo) {  
  # options("rlib_restart_package_not_found" = TRUE)
  package <- strsplit(repo, "/")[[1]] |> 
    {\(parts) {
      if (length(parts) == 2) {
        return(parts[2])
      } else {
        stop("Invalid repo. The repo parameter should be with the 'user/package' format.")
      }
    }}()
  pkg <- paste0(package, " (>= ", jrrosell::package_github_version(repo),")")  
  rlang::check_installed(pkg, action = \(pkg, ...) pak::pak(repo)) 
}
