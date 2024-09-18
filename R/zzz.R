.onLoad <- function(libname, pkgname) {
  assign("jrrosell_ROBOTO_CONDENSED", "Roboto Condensed", envir = topenv())
  assign("jrrosell_ROBOTO_BLACK", "Roboto Black", envir = topenv())
  if (.Platform$OS.type == "windows" && tools::find_gs_cmd() != "") {
    warning("'ghostscript' is required but not found")
    assign("jrrosell_ROBOTO_CONDENSED", "Arial", envir = topenv())
    assign("jrrosell_ROBOTO_BLACK", "Arial Black", envir = topenv())
  }
  n_roboto <-
    extrafont::fonts() |>
    purrr::keep(\(x) stringi::stri_detect(x, regex = "Roboto")) |>
    length()
  if (n_roboto == 0) {
    extrafont::font_import(prompt = FALSE, pattern = "Roboto")
  }
  extrafont::loadfonts(device = "all", quiet = TRUE)
}
