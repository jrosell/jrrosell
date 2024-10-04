#' Detect cores that could be used
#'
#' Select cores in max/min of the available cores.
#'
#' @keywords parallel
#' @rdname detect_cores
#' @param max An integer with the max desired cores (default 10)
#' @param min An integer with the min desired cores (default 2)
#'
#' @details The detect_cores function uses parallelly
#' package. It returns the desired max cores if available or it fails if not
#' min cores are available.
#'
#' @examples
#' cores <- detect_cores(max = 5, min = 1)
#' print(cores)
#' if (FALSE) {
#'   library(jrrosell)
#'   library(future)
#'   plan(multisession, workers = detect_cores(max = 10, min = 2))
#'   plan(sequential)
#' }
#' @export
detect_cores <- function(max = 10, min = 2) {
  if (!requireNamespace("parallelly", quietly = TRUE)) stop("parallelly package is required")
  available <- parallelly::availableCores()
  asked <- min(available, max)
  if (asked <= min) stop(paste0("Not ", asked, " cores available. ", available, " available, but min ", min, " are required."))
  asked
}
