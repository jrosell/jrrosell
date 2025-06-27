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
#' min cores are available (excluding parallelly.availableCores.omit reserved cores or 1 if not defined).
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
  available <- parallelly::availableCores(omit = getOption("parallelly.availableCores.omit", 1L))
  requested <- min(available, max)
  if (requested < min) {
    stop(paste0(
      "Only ", available, " usable core(s) available. ",
      "Minimum required is ", min, ". ",
      "Consider reducing 'min' or freeing system resources."
    ))
  }
  requested
}


#' Request the maximum safe number of cores
#'
#' When parallizing within resamples, required memory can crash the system.
#'
#' @keywords parallel
#' @rdname request_max_safe_cores
#' @param estimated_max_rss butes of maximum rss it will used (You can get it from syrup package)
#' @param memory_usage the proportion of the system memory that will be used (0.8)
#' @param verbose to debug (TRUE)
#'
#' @details The detect_cores function uses parallelly
#' package. It returns the desired max cores if available or it fails if not
#' min cores are available (excluding system reserved cores).
#'
#' @exportPattern pattern
request_max_safe_cores_from_rss <- function(
    estimated_max_rss,
    memory_usage = 0.5,
    verbose = TRUE) {
  if (!requireNamespace("parallelly", quietly = TRUE)) {
    stop("The 'parallelly' package is required. Please install it with install.packages('parallelly').")
  }
  total_cores <- detect_cores(50, 2)

  total_mem_gb <- tryCatch(
    {
      if (.Platform$OS.type == "unix") {
        mem_kb <- as.numeric(system("awk '/MemTotal/ {print $2}' /proc/meminfo", intern = TRUE))
        mem_kb / 1024^2
      } else if (.Platform$OS.type == "windows") {
        memory.size(max = TRUE) / 1024
      } else {
        NA
      }
    },
    error = function(e) NA
  )

  if (is.na(total_mem_gb)) {
    warning("Unable to detect system memory. Defaulting to 1 core.")
    return(1)
  }

  # Convert to MB (assuming bytes input)
  max_rss_mb <- estimated_max_rss / 1024^2

  usable_mem_mb <- total_mem_gb * 1024 * memory_usage
  max_parallel_by_memory <- floor(usable_mem_mb / max_rss_mb)

  safe_cores <- max(1, min(total_cores, max_parallel_by_memory))

  if (verbose) {
    cat("Estimated max RSS per task:", round(max_rss_mb, 2), "MB\n")
    cat("Total system memory:", round(total_mem_gb, 2), "GB\n")
    cat("Available cores (CPU):", total_cores, "\n")
    cat("Max cores safe by memory:", max_parallel_by_memory, "\n")
    cat("-> Returning safe core count:", safe_cores, "\n")
  }

  return(safe_cores)
}
