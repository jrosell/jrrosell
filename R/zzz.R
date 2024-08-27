.onLoad <- function(libname, pkgname) {
  # extrafont::font_import()
  if (rlang::is_installed("extrafont")) {
    extrafont::loadfonts(device = "all", quiet = TRUE)
  }
}
