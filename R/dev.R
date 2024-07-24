
#' Internal helper function for package development
#' @examples
#' if (FALSE) {
#'   devtools::load_all(); rebuild_docs_and_check()
#' }
#' 
#' @keywords internal
rebuild_docs_and_check <- function() {
    # usethis::use_description(list(
    #     "Title" = "Personal R package for Jordi Rosell",
    #     "Description" = "Useful functions for personal usage.",
    #     "Authors@R" = utils::person(
    #         "Jordi", "Rosell",
    #         email = "jroselln@gmail.com",
    #         role = c("aut", "cre"),
    #         comment = c(ORCID = "0000-0002-4349-1458")
    #     ),
    #     Language =  "en"
    # ))
    usethis::use_cc0_license()
    packages <- c(
        "sf",
        "doParallel",
        "httr",
        "httr2",
        "janitor",
        "parallel",
        "parallelly",
        "parsnip",
        "purrr",
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
        "glue"
    )
    packages |> purrr::map(
        \(x){usethis::use_package(x, type = "Suggests"); x}
    )
    devtools::load_all()
    usethis::use_namespace()
    devtools::document()
    pkgdown::build_site()
    devtools::check()
}
