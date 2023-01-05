#' Start parallel processing
#'
#' Start timer and parallel processing in max/min of the available cores.
#'
#' @rdname startParallel
#' @param msg A character vector with the tictoc timer message.
#' @param max An integer with the max desired cores.
#' @param min An integer with the min desired cores.
#'
#' @details The startParallel function uses tictoc, parallelly, parallel
#' packages. It returns a parallel cluster to be passed as a stopParallel
#' argument
#'
#' @examples
#' cl <- startParallel("parallel processing", max = 10, min = 2)
#' print("parallel processing here")
#' stopParallel(cl)
#'
#' @export
startParallel <- function(msg = NULL, max = 10, min = 2) {
    available <- parallelly::availableCores()
    ask <- min(available, max)
    if (ask <= min) stop("Not enough cores available")
    cl <- parallel::makeCluster(ask) # makeCluster, makePSOCKcluster, makeForkCluster
    doParallel::registerDoParallel(cl)
    tictoc::tic(msg)
    cl
}

#' Stop parallel processing
#'
#' Stop started timer and parallel processing.
#'
#' @rdname stopParallel
#' @param cl A cluster object returned by startParallel or parallel::makeCluster, parallel::makePSOCKcluster, parallel::makeForkCluster
#'
#' @details The stopParallel function uses tictoc and  parallel
#' packages. It returns the result of parallel::stopCluster(cl) method.
#'
#' @examples
#' cl <- startParallel()
#' print("parallel processing here")
#' stopParallel(cl)
#'
#' @export
stopParallel <- function(cl) {
    tictoc::toc()
    parallel::stopCluster(cl)
}
