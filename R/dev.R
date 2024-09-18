#' Internal helper function for package development
#' @examples
#' if (FALSE) {
#'   devtools::load_all(); rebuild_package_and_check()
#'   devtools::load_all(); rebuild_package_and_check(build_site = TRUE)
#' }
#'
#' @noRd
rebuild_package_and_check <- function(build_site = FALSE) {
  previous_version <- package_github_version("jrosell/jrrosell")
  usethis::use_description(list(
    "Title" = "Personal R package for Jordi Rosell",
    "Description" = "Useful functions for personal usage.",
    "Version" = previous_version,
    "Authors@R" = utils::person(
      "Jordi", "Rosell",
      email = "jroselln@gmail.com",
      role = c("aut", "cre"),
      comment = c(ORCID = "0000-0002-4349-1458")
    ),
    Language = "en"
  ))
  # usethis::use_pkgdown_github_pages()
  usethis::use_package("R", type = "Depends", min_version = "4.3.0")
  write(
    "URL: https://jrosell.github.io/jrrosell, https://github.com/jrosell/jrrosell",
    here::here("DESCRIPTION"),
    append = TRUE
  )
  usethis::use_cc0_license()
  suggests_packages <- c(
    "styler",
    "sf",
    "doParallel",
    "httr",
    "httr2",
    "janitor",
    "parallel",
    "parallelly",
    "parsnip",
    "readr",
    "tidymodels",
    "recipes",
    "tibble",
    "tictoc",
    "workflows",
    "openxlsx",
    "hardhat",
    "glmnet",
    "xgboost",
    "devtools",
    "pkgdown",
    "testthat",
    "modeldata",
    "usethis",
    "future",
    "doFuture",
    "dplyr",
    "tune",
    "blastula",
    "beepr",
    "glue",
    "here",
    "ggplot2",
    "extrafont",
    "pak"
  )
  suggests_packages |> purrr::map(
    \(x){
      usethis::use_package(x, type = "Suggests")
      x
    }
  )
  imports_packages <- c(
    "rlang", "extrafont", "purrr", "stringi"
  )
  imports_packages |> purrr::map(
    \(x){
      usethis::use_package(x, type = "Imports")
      x
    }
  )
  spain_ccaas <- readr::read_rds("inst/extdata/spain_ccaas.rds")
  spain_provinces <- readr::read_rds("inst/extdata/spain_provinces.rds")
  usethis::use_data(spain_ccaas, spain_provinces, overwrite = TRUE)
  styler::style_pkg(exclude_files = c("R/RcppExports\\.R", "R/cpp11\\.R", "R/import-standalone.*\\.R", "R/dev\\.R"))
  devtools::load_all()
  devtools::document()
  devtools::check(document = FALSE)  
  if(build_site == TRUE) {    
    pkgdown::build_site(preview = FALSE) # # usethis::use_pkgdown_github_pages()
    utils::browseURL(here::here("docs", "index.html"), browser = getOption("browser")) 
  }  
  # usethis::use_version(which = "dev", push = FALSE)
}