
#' Prep, juice and glimpse a recipe or workflow
#'
#' Stop started timer and parallel processing.
#'
#' @rdname prep_juice
#' @param object A recipe or a workflow object with a recipe
#'
#' @details The stopParallel function uses tictoc and  parallel
#' packages. It returns the result of parallel::stopCluster(cl) method.
#'
#' @examples
#' recipes::recipe(spray ~ ., data = InsectSprays) |>
#'    prep_juice()
#' recipes::recipe(spray ~ ., data = InsectSprays)  |>
#'    workflows::workflow(parsnip::linear_reg()) |>
#'    prep_juice()
#'
#' @seealso https://github.com/jrosell/jrrosell/blob/main/R/parallel.R
#' @export
prep_juice <- function(object) {
    if(!requireNamespace("recipes", quietly = TRUE)) stop("recipes package is required")
    if(!requireNamespace("tibble", quietly = TRUE)) stop("tibble package is required")
    if (inherits(object, "workflow")) {
        object <- object |>  workflows::extract_preprocessor()
    }
    object |> recipes::prep() |> recipes::juice() |> tibble::glimpse()
}
