#' Calculate split size
#'
#' From binary classification problems, with the desired std_err it returns the
#' minimal assesment/validation set size.
#'
#' @keywords tidymodels
#' @rdname calc_split_size
#' @param std_err The desired std_err numeric (default NULL)
#' @param confidence_interval (default 0.95)
#' @param margin_error (default 0.02)
#'
#' @details The calc_validation_size function returns the minimal validation size for expected probabilities and desired error.
#' s
#' @examples
#' calc_split_size()
#' calc_split_size(confidence_interval = 0.95, margin_error = 0.02)
#' calc_split_size(std_err = 0.02)
#'
#' @source <https://stats.stackexchange.com/a/304996/7387>
#' @export
calc_split_size <- function(std_err = NULL, confidence_interval = 0.95, margin_error = 0.02) {
  expected_p <- 0.5
  if (is.null(std_err)) {
    alpha <- 1 - confidence_interval
    z <- qnorm(1 - alpha / 2)
    std_err <- margin_error / z
    cat(glue::glue("Info: {std_err} std_err is used.\n\n"))
  }
  (expected_p - expected_p^2) / (std_err^2)
}

#' Calculate split proportion
#'
#' From a data frame, it returns the minimal split proportion for validation.
#'
#' @keywords tidymodels
#' @rdname calc_split_prop
#' @param df A data frame
#' @param k number of desired folds (default 10)
#'
#' @details The calc_validation_size function returns the optimal split
#' proportion according to the number of rows for your validation set.
#'
#' @examples
#' calc_split_prop(data.frame(row = 1:891))
#'
#' @source <https://stats.stackexchange.com/a/305063/7387>
#' @export
calc_split_prop <- function(df, k = 10) {
  total <- nrow(df)
  total <- rlang::int(total)
  k <- rlang::int(k)
  min_prop <- (1 / sqrt(2 * total))
  rows <- round(total * min_prop)
  fold_total <- round((total - rows) / k)
  fold_min_prop <- (1 / sqrt(2 * fold_total))
  fold_rows <- round(fold_total * fold_min_prop)
  cat(glue::glue("Info: Minimal validation split of {rows}/{total} rows.\n\n"))
  cat(glue::glue("Info: Resamples of {fold_rows}/{fold_total} rows for {k} of folds.\n\n"))
  return(min_prop)
}
