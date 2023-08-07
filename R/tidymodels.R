
#' Prep, juice and glimpse a recipe or workflow
#'
#' Stop started timer and parallel processing.
#'
#' @rdname prep_juice
#' @param object A recipe or a workflow object with a recipe
#'
#' @examples
#' recipes::recipe(spray ~ ., data = InsectSprays) |>
#'    prep_juice()
#' recipes::recipe(spray ~ ., data = InsectSprays)  |>
#'    workflows::workflow(parsnip::linear_reg()) |>
#'    prep_juice()
#'
#' @seealso https://github.com/jrosell/jrrosell/blob/main/R/tidymodels.R
#' @export
prep_juice <- function(object) {
    if(!requireNamespace("recipes", quietly = TRUE)) stop("recipes package is required")
    if(!requireNamespace("tibble", quietly = TRUE)) stop("tibble package is required")
    if (inherits(object, "workflow")) {
        object <- object |>  workflows::extract_preprocessor()
    }
    object |> recipes::prep() |> recipes::juice() |> tibble::glimpse()
}


#' Update recipe step values by id
#'
#' Update the vaules of a specific recipe step located by id
#'
#' @rdname update_step
#' @param object A recipe or a workflow object with a recipe
#'
#' @examples
#' recipes::recipe(spray ~ ., data = InsectSprays) |>
#'    recipes::step_ns(count, deg_free = hardhat::tune(), id="ns") |>
#'    update_step("ns", deg_free = 1)
#'
#' @seealso https://github.com/jrosell/jrrosell/blob/main/R/tidymodels.R
#' @export
update_step <- function(object, target_id, ...) {
    if(!requireNamespace("recipes", quietly = TRUE)) stop("recipes package is required")
    if (inherits(object, "workflow")) {
        object <- object |>  workflows::extract_preprocessor()
    }
    matching_index <- which(sapply(object$steps, function(step) step$id == target_id))
    if (length(matching_index) == 1) {
        index_to_update <- matching_index[1]
        object$steps[[index_to_update]] <- update(object$steps[[index_to_update]], ...)
    }
    return(object)
}
