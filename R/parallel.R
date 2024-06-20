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
#' cl <- startParallel("parallel processing", max = 3, min = 1)
#' print("parallel processing here")
#' stopParallel(cl)
#'
#' @seealso https://github.com/jrosell/jrrosell/blob/main/R/parallel.R
#' @export
startParallel <- function(msg = NULL, max = 10, min = 1) {
    if(!requireNamespace("parallel", quietly = TRUE)) stop("parallel package is required")
    if(!requireNamespace("parallelly", quietly = TRUE)) stop("parallelly package is required")
    if(!requireNamespace("doParallel", quietly = TRUE)) stop("doParallel package is required")
    if(!requireNamespace("doFuture", quietly = TRUE)) stop("doFuture package is required")
    if(!requireNamespace("tictoc", quietly = TRUE)) stop("doParallel package is required")

    available <- parallelly::availableCores()
    ask <- min(available, max)
    if (ask <= min) stop(paste0("Not enough cores available:", available, " available, but min ", min, " required."))
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
#' @seealso https://github.com/jrosell/jrrosell/blob/main/R/parallel.R
#' @export
stopParallel <- function(cl) {
    if(!requireNamespace("tictoc", quietly = TRUE)) stop("tictoc package is required")
    if(!requireNamespace("parallel", quietly = TRUE)) stop("parallel package is required")

    tictoc::toc()
    parallel::stopCluster(cl)
}

#' Map parallel processing
#'
#' Map data over a function in one parallel core
#'
#' @rdname mapParallel
#' @param x An object
#' @param fun_list A list of functions to run in parallel over the object
#'
#' @details The mapParallel function uses parallel
#' package to run functions to run in parallel over the object. It doesn't returning anything.
#'
#' @examples
#' data.frame(x = 2) |> mapParallel(list(function(x) print(x), function(x) print(x*2)))
#'
#' @seealso <https://github.com/jrosell/jrrosell/blob/main/R/parallel.R>
#' @export
mapParallel <- function(x, fun_list) {
    if(!requireNamespace("parallel", quietly = TRUE)) stop("parallel package is required")

    for (fun in fun_list) {
        x |>
            list() |>
            parallel::mclapply(fun, mc.cores = 1)
    }
}
