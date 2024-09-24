#' @noRd
.onLoad <- function(libname, pkgname) {
  sysfonts::font_add_google("Roboto")
  sysfonts::font_add_google("Roboto Condensed")
  showtext::showtext_auto()
  n_roboto <-
    extrafont::fonts() |>
    purrr::keep(\(x) stringi::stri_detect(x, regex = "Roboto")) |>
    length()
  if (n_roboto == 0) {
    extrafont::font_import(prompt = FALSE)
  }
  extrafont::loadfonts(device = "all", quiet = TRUE)
}

#' @noRd
ignore_unused_imports <- function() {
  stop("Do not use.")
  curl::curl
  jsonlite::base64_dec
}
