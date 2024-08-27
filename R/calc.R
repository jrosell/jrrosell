#' Calculate validation size
#'
#' From and expected p and desired std_err, it returns the
#' minimal validation size for your assesment sets or validation sets.
#'
#' @keywords tidymodels
#' @rdname calc_validation_size
#' @param expected_p An object
#' @param desired_std_err An expresion
#'
#' @details The calc_validation_size function returns the minimal validation size for expected probabilities and desired error.
#'
#' @examples
#' calc_validation_size(expected_p = 0.8, desired_std_err = 0.02)
#'
#' @source <https://stats.stackexchange.com/a/304996/7387>
#' @seealso <https://github.com/jrosell/jrrosell/blob/main/R/calc.R>
#' @export
calc_validation_size <- function(expected_p, desired_std_err) {
  (expected_p - expected_p^2) / (desired_std_err^2)
}

#' Calculate split proportion
#'
#' From a data frame, it returns the split proportion.
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
#' @seealso <https://github.com/jrosell/jrrosell/blob/main/R/calc.R>
#' @export
calc_split_prop <- function(df) {
  n <- nrow(df)
  return(1 / sqrt(2 * n))
}
