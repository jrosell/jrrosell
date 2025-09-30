#' Internal helper function for package development
#' @examples
#' if (FALSE) {
#'   devtools::load_all(); rebuild_package_and_check(build_site = FALSE)
#'   # usethis::use_import_from(package = "stats", fun = "qnorm")
#'   # usethis::use_import_from(package = "utils", fun = "memory.size")
#'   rhub::rhub_check(platforms = "windows", r_versions = "4.5")
#'   usethis::use_version(which = "dev", push = FALSE)
#'   usethis::use_github_release()
#' }
#'
#' @noRd
rebuild_package_and_check <- function(build_site = FALSE) {
  previous_version <- "0.0.0.9014" #package_github_version("jrosell/jrrosell")
  if (build_site == TRUE) {
    usethis::use_description(list(
      "Title" = "Personal R package for Jordi Rosell",
      "Description" = "Useful functions for personal usage.",
      "Version" = previous_version,
      "Authors@R" = utils::person(
        "Jordi",
        "Rosell",
        email = "jroselln@gmail.com",
        role = c("aut", "cre"),
        comment = c(ORCID = "0000-0002-4349-1458")
      ),
      "URL" = "https://jrosell.github.io/jrrosell",
      "BugReports" = "https://github.com/jrosell/jrrosell/issues",
      Language = "en"
    ))
  }
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
    "httr2",
    "janitor",
    "parallel",
    "parsnip",
    "readr",
    "forcats",
    "tibble",
    "tictoc",
    "workflows",
    "hardhat",
    "glmnet",
    "devtools",
    "pkgdown",
    "testthat",
    "usethis",
    "future",
    "doFuture",
    "dplyr",
    "blastula",
    "beepr",
    "glue",
    "here",
    "ggplot2",
    "yyjsonr",
    "webfakes",
    "spacyr",
    "udpipe",
    "rstudioapi",
    "pak",
    "ellmer",
    "stringr",
    # "broom",
    # "modeldata",
    # "rsample",
    # "xgboost",
    # "yardstick",
    "rcmdcheck"
  )
  suggests_packages |>
    purrr::walk(\(x) usethis::use_package(x, type = "Suggests"))

  imports_packages <- c(
    "rlang",
    "showtext",
    "sysfonts",
    "extrafont",
    "purrr",
    "stringi",
    "jsonlite",
    "curl",
    "scales",
    "tidyr",
    "stringdist",
    "tm",
    "openxlsx",
    "recipes",
    "tune",
    "workflows",
    "parsnip",
    "parallelly"
  )
  imports_packages |>
    purrr::walk(\(x) usethis::use_package(x, type = "Imports"))

  spain_ccaas <- readr::read_rds("inst/extdata/spain_ccaas.rds")
  spain_provinces <- readr::read_rds("inst/extdata/spain_provinces.rds")
  usethis::use_data(spain_ccaas, spain_provinces, overwrite = TRUE)
  usethis::use_import_from("rlang", c(".data", ".env"))  # usethis::use_package_doc()
  if (build_site == TRUE) {
    devtools::check()
    pkgdown::build_site(preview = FALSE) # usethis::use_pkgdown_github_pages()
    utils::browseURL(
      here::here("docs", "index.html"),
      browser = getOption("browser")
    )
  } else {
    devtools::check()
  }
}


#' Generate documentation
#'
#' @description
#' It returns the genereated documentation from the selected model
#'
#' @param x A single string.
#' @param model A single string with the ollama model to use.
#'
#' @returns
#' Generated documentation as a character string.
#'
#' @keywords packages
#' @rdname generate_documentation
#' @export
generate_documentation <- \(x, model = "qwen2.5-coder:3b") {
  chat_session <- ellmer::chat_ollama(
    system_prompt = readLines("inst/prompts/prompt-roxygen2.md"),
    model = model
  )
  raw_doc <- chat_session$chat(glue::glue("{x}"))
  generated_doc <- raw_doc |>
    stringr::str_replace_all("\n\n", "\n") |>
    stringr::str_replace_all("\r\n\r\n", "\r\n")
  generated_doc
}


#' Adding to generate documentation
#'
#' @description
#' It changes the selected code for the the generated documentation using
#' the configured ollama model.
#'
#' @param model A single string with the ollama model to use.
#' @param context the IDE context. Defaults to rstudioapi::getActiveDocumentContext
#'
#' @returns
#' Nothing.
#'
#' @keywords packages
#' @rdname addin_generate_documentation
#' @export
addin_generate_documentation <- function(
  model = "qwen2.5-coder:3b",
  context = rstudioapi::getActiveDocumentContext()
) {
  selection <- rstudioapi::primary_selection(context)
  if (selection[["text"]] == "") {
    # It's good practice to stop if nothing is selected
    stop(
      "No code selected. Please highlight the function code first.",
      call. = FALSE
    )
  }
  selected_text <- selection$text
  cat(paste0("Generating documentation...", "\n"))
  generated_doc <- generate_documentation(
    glue::glue("{selected_text}"),
    model = model
  ) |>
    # Your original cleanup steps
    stringr::str_replace_all("\n\n", "\n") |>
    stringr::str_replace_all("\r\n\r\n", "\r\n")
  rstudioapi::modifyRange(
    location = selection$range,
    text = paste0(generated_doc, "\n", selected_text),
    id = context$id
  )
}
