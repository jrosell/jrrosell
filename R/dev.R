#' Internal helper function for package development
#' @examples
#' if (FALSE) {
#'   devtools::load_all(); rebuild_package_and_check(build_site = TRUE)
#'   usethis::use_import_from(package = "stats", fun = "qnorm")
#'   rhub::rhub_check(platforms = "windows", r_versions = "4.5")
#'   usethis::use_version(which = "dev", push = PUSH)
#'   usethis::use_github_release()
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
    "URL" = "https://jrosell.github.io/jrrosell",
    "BugReports" = "https://github.com/jrosell/jrrosell/issues",
    Language = "en"
  ))
  # usethis::use_pkgdown_github_pages()
  usethis::use_package("R", type = "Depends", min_version = "4.3.0")    
  usethis::use_cc0_license()
  suggests_packages <- c(
    "styler",
    "sf",
    "fst",
    "stacks",
    "workflowsets",
    "doParallel",
    "httr",
    "httr2",
    "janitor",
    "parallel",
    "parallelly",
    "parsnip",
    "readr",
    "forcats",
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
    "yyjsonr",
    "webfakes",
    "pak"
  )
  suggests_packages |>
    purrr::walk(\(x) usethis::use_package(x, type = "Suggests"))
  
  imports_packages <- c(
    "rlang", "showtext", "sysfonts", "extrafont", "purrr", "stringi", "jsonlite", "curl", "scales", "tidyr", "stringdist", "tm"
  )
  imports_packages |>
    purrr::walk(\(x) usethis::use_package(x, type = "Imports"))

  spain_ccaas <- readr::read_rds("inst/extdata/spain_ccaas.rds")
  spain_provinces <- readr::read_rds("inst/extdata/spain_provinces.rds")
  usethis::use_data(spain_ccaas, spain_provinces, overwrite = TRUE)
  # usethis::use_package_doc()
  usethis::use_import_from("rlang", c(".data", ".env"))
  styler::style_pkg(exclude_files = c("R/RcppExports\\.R", "R/cpp11\\.R", "R/import-standalone.*\\.R", "R/dev\\.R"))
  devtools::load_all()
  devtools::document()
  devtools::check(document = FALSE) # rcmdcheck::rcmdcheck(repos = FALSE).
  if(build_site == TRUE) {    
    pkgdown::build_site(preview = FALSE) # # usethis::use_pkgdown_github_pages()
    utils::browseURL(here::here("docs", "index.html"), browser = getOption("browser")) 
  }  
}
