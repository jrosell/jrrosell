#' Sets a dark blue colored dark minimal theme using the Roboto font family
#'
#' @rdname theme_set_roboto_darkblue
#' @keywords graphing
#' @param ... Other parameters passed to theme_set
#' @examples
#' library(jrrosell)
#' library(ggplot2)
#' theme_set_roboto_darkblue()
#' ggplot(iris, aes(Species)) +
#'   geom_bar()
#' @export
theme_set_roboto_darkblue <- function(...) {
  ggplot2::update_geom_defaults("rect", list(fill = "darkblue", alpha = 0.8))
  ggplot2::update_geom_defaults("line", list(color = "darkblue", alpha = 0.8))
  ggplot2::update_geom_defaults("point", list(color = "darkblue", alpha = 0.8))
  ggplot2::update_geom_defaults("bar", list(fill = "darkblue", alpha = 0.8))
  ggplot2::theme_set(theme_roboto(...))
}

#' Sets a minimal theme using the Roboto font family
#'
#' It requires roboto fonts installed in your O.S. and run z
#'
#' @rdname theme_roboto
#' @keywords graphing
#' @param base_size = 11
#' @param strip_text_size = 12
#' @param strip_text_margin = 5
#' @param subtitle_size = 13
#' @param subtitle_margin = 10
#' @param plot_title_size = 16
#' @param plot_title_margin = 10
#' @param ... Other parameters passed to theme_set
#' @examples
#' library(jrrosell)
#' library(ggplot2)
#' theme_set(theme_roboto())
#' ggplot(iris, aes(Species)) +
#'   geom_bar()
#' @export
theme_roboto <- function(
    base_size = 13,
    strip_text_size = 14,
    strip_text_margin = 6,
    subtitle_size = 14,
    subtitle_margin = 10,
    plot_title_size = 18,
    plot_title_margin = 12,
    ...) {
  ret <- ggplot2::theme_minimal(
    base_family = "Roboto Condensed",
    base_size = base_size, ...
  )
  ret$strip.text <- ggplot2::element_text(
    hjust = 0, size = strip_text_size,
    margin = ggplot2::margin(b = strip_text_margin),
    family = "Roboto Condensed"
  )
  ret$plot.subtitle <- ggplot2::element_text(
    hjust = 0, size = subtitle_size,
    margin = ggplot2::margin(b = subtitle_margin),
    family = "Roboto Condensed"
  )
  ret$plot.title <- ggplot2::element_text(
    hjust = 0, size = plot_title_size,
    margin = ggplot2::margin(b = plot_title_margin),
    family = "Roboto Black"
  )
  ret
}
