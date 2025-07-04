#' @noRd
.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Attaching package: 'jrrosell'")

  if (!.jrrosell_env$roboto_installed) {
    packageStartupMessage("Roboto fonts not found in extrafont.")
    packageStartupMessage("Run `extrafont::font_import()` manually if needed.")
  }
}

#' @noRd
.onLoad <- function(libname, pkgname) {
  try({
    sysfonts::font_add_google("Roboto")
    sysfonts::font_add_google("Roboto Condensed")
    showtext::showtext_auto()
  })
  .jrrosell_env$roboto_installed <- FALSE
  n_roboto <-
    extrafont::fonts() |>
    purrr::keep(\(x) stringi::stri_detect(x, regex = "Roboto")) |>
    length()
  if (n_roboto > 0) {
    .jrrosell_env$roboto_installed <- TRUE
  }
  extrafont::loadfonts(device = "all", quiet = TRUE)
}

#' @noRd
ignore_unused_imports <- function() {
  stop("Do not use.")
  curl::curl
  jsonlite::base64_dec
}

.jrrosell_env <- new.env(parent = emptyenv())
assign("spacy_initialized", FALSE, envir = .jrrosell_env)
