utils::globalVariables(c(
  "ROBOTO_CONDENSED", "ROBOTO_BLACK"
))

.onLoad <- function(libname, pkgname) {
  # extrafont::font_import()
  if (!rlang::is_installed("extrafont")) stop("extrafont package is required")

  ROBOTO_CONDENSED <<- "Roboto Condensed"
  ROBOTO_BLACK <<- "Roboto Black"
  if (.Platform$OS.type == "windows" && tools::find_gs_cmd() != "") {
    warning("'ghostscript' is required but not found")
     ROBOTO_CONDENSED <<- "Arial"
     ROBOTO_BLACK <<- "Arial Black"
  }    
  n_roboto <-
    extrafont::fonts() |>
    purrr::keep(\(x) stringr::str_detect(x, "Roboto")) |>
    length()
  if (n_roboto == 0) {
    extrafont::font_import(prompt = FALSE, pattern = "Roboto")
  }
  extrafont::loadfonts(device = "all", quiet = TRUE)
}
