.onLoad <- function(libname, pkgname) {
  sysfonts::font_add("Roboto Condensed", regular = "RobotoCondensed-Regular.ttf")
  sysfonts::font_add("Roboto Black", regular = "Roboto-Black.ttf")
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
