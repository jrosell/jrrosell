#' Calculate split size
#'
#' From binary classification problems, with the desired std_err it returns the
#' minimal assesment/validation set size.
#'
#' @keywords tidymodels
#' @rdname calc_split_size
#' @param std_err The desired std_err numeric (default 0.001)
#'
#' @details The calc_validation_size function returns the minimal validation size for expected probabilities and desired error.
#' s
#' @examples
#' calc_split_size()
#' calc_split_size(std_err = 0.02)
#'
#' @source <https://stats.stackexchange.com/a/304996/7387>
#' @export
calc_split_size <- function(std_err = 0.001) {
  expected_p <- 0.5
  (expected_p - expected_p^2) / (std_err^2)
}

#' Calculate split proportion
#'
#' From a data frame, it returns the minimal split proportion for validation.
#'
#' @keywords tidymodels
#' @rdname calc_split_prop
#' @param df A data frame
#'
#' @details The calc_validation_size function returns the optimal split
#' proportion according to the number of rows for your validation set.
#'
#' @examples
#' calc_split_prop(data.frame(row = 1:891))
#'
#' @source <https://stats.stackexchange.com/a/305063/7387>
#' @export
calc_split_prop <- function(df) {
  n <- nrow(df)
  return(1 / sqrt(2 * n))
}
