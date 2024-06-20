
#' Do the last fit and get the metrics
#'
#' @keywords tidymodels
#' @rdname last_metrics
#' @param res Tune results
#' @param split The initial split object
#' @param metric What metric to use to select the best workflow
#'
#' @examples
#' library(tidymodels)
#' library(xgboost)
#' library(modeldata)
#' data(cells)
#' split <- cells |>
#'   mutate(across(where(is.character), as.factor)) |>
#'   sample_n(500) |>
#'   initial_split(strata = class)
#' train <- training(split)
#' folds <- vfold_cv(train, v = 2, strata = class)
#' wf <- train |>
#'   recipe(case ~ .) |>
#'   step_integer(all_nominal_predictors())  |>
#'   workflow_boost_tree()
#' res <- wf |>
#'   tune_grid(
#'     folds,
#'     grid = 2,
#'     metrics = metric_set(roc_auc),
#'     control = control_grid(save_workflow = TRUE, verbose = FALSE)
#'   )
#' res |> collect_metrics()
#' res |> last_metrics(split, "roc_auc")
#' best <- res |> fit_best()
#' best |>
#'   augment(testing(split)) |>
#'   roc_auc(case, .pred_Test) |>
#'   pull(.estimate)
#'
#' @seealso https://github.com/jrosell/jrrosell/blob/main/R/tidymodels.R
#' @export
last_metrics <- function(res, split, metric) {
    wf <- res |> tune::extract_workflow()
    wf |>
        tune::finalize_workflow(tune::select_best(res, metric = metric)) |>
        tune::last_fit(split) |>
        tune::collect_metrics()
}

#' Prep, juice and glimpse a recipe or workflow
#'
#' @keywords tidymodels
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
#' @source <https://recipes.tidymodels.org/reference/update.step.html>
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

#' Prep, juice and get cols from a recipe or workflow
#'
#' @keywords tidymodels
#' @rdname prep_juice_cols
#' @param object A recipe or a workflow object with a recipe
#'
#' @examples
#' recipes::recipe(spray ~ ., data = InsectSprays) |>
#'    prep_juice_cols()
#'
#' @seealso https://github.com/jrosell/jrrosell/blob/main/R/tidymodels.R
#' @export
prep_juice_cols <- function(object) {
    if(!requireNamespace("recipes", quietly = TRUE)) stop("recipes package is required")
    if(!requireNamespace("tibble", quietly = TRUE)) stop("tibble package is required")
    if (inherits(object, "workflow")) {
        object <- object |>  workflows::extract_preprocessor()
    }
    object |> recipes::prep() |> recipes::juice() |>  dim() |> _[2]
}


#' Update recipe step values by id
#'
#' Update the vaules of a specific recipe step located by id
#'
#' @keywords tidymodels
#' @importFrom stats update
#' @rdname update_step
#' @param object A recipe or a workflow object with a recipe
#' @param target_id The id name of the step
#' @param ... The arguments to update the step.
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



#' Create an xgboost tunable workflow for regression and classification
#'
#' @keywords tidymodels
#' @rdname workflow_boost_tree
#' @param rec prerocessing recipe to build the workflow
#' @param engine optional, xgboost by default
#' @param counts Optional logic argument wether mtry use counts or not
#' @param ... optional engine arguments
#'
#' @examples
#' library(tidymodels)
#' library(xgboost)
#' library(modeldata)
#' library(future)
#' data(cells)
#' split <- cells |>
#'   mutate(across(where(is.character), as.factor)) |>
#'   sample_n(500) |>
#'   initial_split(strata = class)
#' train <- training(split)
#' folds <- vfold_cv(train, v = 2, strata = class)
#' wf <- train |>
#'   recipe(case ~ .) |>
#'   step_integer(all_nominal_predictors())  |>
#'   workflow_boost_tree()
#' doFuture::registerDoFuture()
#' plan(sequential)
#' res <- wf |>
#'   tune_grid(
#'     folds,
#'     grid = 2,
#'     metrics = metric_set(roc_auc),
#'     control = control_grid(save_workflow = TRUE, verbose = FALSE)
#'   )
#' res |> collect_metrics()
#' res |> last_metrics(split, "roc_auc")
#' best <- res |> fit_best()
#' best |>
#'   augment(testing(split)) |>
#'   roc_auc(case, .pred_Test) |>
#'   pull(.estimate)
#'
#' @seealso https://github.com/jrosell/jrrosell/blob/main/R/tidymodels.R
#' @export
workflow_boost_tree <- function(rec, engine = "xgboost", counts = TRUE, ...) {
    recipe_mode <- .get_recipe_mode(rec)
    mode <- recipe_mode$mode
    rec |>
        workflows::workflow(
            parsnip::boost_tree(mode, engine) |>
                parsnip::set_args(
                trees = parsnip::tune(), learn_rate = parsnip::tune(), # steps and step size
                tree_depth = parsnip::tune(), min_n = parsnip::tune(), loss_reduction = parsnip::tune(), # complexity
                sample_size = parsnip::tune(), mtry = parsnip::tune(), # randomness
                counts = !!counts, # engine specific
                ...
            )
        )
}


#' Create a tuneable glmnet worfklow for regression and classification
#'
#' @keywords tidymodels
#' @rdname workflow_elasticnet
#' @param rec prerocessing recipe to build the workflow
#' @param engine optional, glmnet by default
#' @param ... Optional engine arguments
#'
#' @examples
#' library(tidymodels)
#' library(glmnet)
#' library(modeldata)
#' library(future)
#' data(cells)
#' split <- cells |>
#'   mutate(across(where(is.character), as.factor)) |>
#'   sample_n(500) |>
#'   initial_split(strata = class)
#' train <- training(split)
#' folds <- vfold_cv(train, v = 2, strata = class)
#' wf <- train |>
#'   recipe(case ~ .) |>
#'   step_integer(all_nominal_predictors())  |>
#'   workflow_elasticnet()
#' doFuture::registerDoFuture()
#' plan(sequential)
#' res <- wf |>
#'   tune_grid(
#'     folds,
#'    grid = 2,
#'    metrics = metric_set(roc_auc),
#'    control = control_grid(save_workflow = TRUE, verbose = FALSE)
#'  )
#' res |> collect_metrics()
#' res |> last_metrics(split, "roc_auc")
#' best <- res |> fit_best()
#' best |>
#'   augment(testing(split)) |>
#'   roc_auc(case, .pred_Test) |>
#'   pull(.estimate)
#'
#' @seealso https://github.com/jrosell/jrrosell/blob/main/R/tidymodels.R
#' @export
#'
workflow_elasticnet  <- function(rec, engine = "glmnet", ...) {
    recipe_mode <- .get_recipe_mode(rec)
    if (recipe_mode$is_reg) {
        print("regression")
        model <- parsnip::linear_reg("regression", engine) |>
            parsnip::set_args(
                penalty = parsnip::tune(),
                mixture = parsnip::tune(),
                ...
            )
    }
    if (recipe_mode$is_binary) {
        print("logistic_reg binary clssification")
        model <- parsnip::logistic_reg("classification", engine) |>
            parsnip::set_args(
                penalty = parsnip::tune(),
                mixture = parsnip::tune(),
                ...
            )
    }
    if(recipe_mode$is_multinom) {
        print("multinom_reg clssification")
        model <- parsnip::multinom_reg("classification", engine) |>
            parsnip::set_args(
                penalty = parsnip::tune(),
                mixture = parsnip::tune(),
                ...
            )
    }
    return(workflows::workflow(rec, model))
}

#' Help infer things about the mode and the model to fit
#' @keywords internal
#' @importFrom rlang .data
.get_recipe_mode <- function(rec) {
    outcome <- rec |> summary() |> dplyr::filter(.data$role == "outcome")|> dplyr::pull(.data$variable)
    outcome_vec <- rec$template[[outcome]]
    n_outcome <- dplyr::n_distinct(outcome_vec)
    is_reg <- class(outcome_vec) == "integer" | class(outcome_vec)  == "double"
    is_multinom <- class(outcome_vec) == "factor" && n_outcome > 2
    is_binary <- class(outcome_vec) == "factor" && n_outcome == 2
    list(
        mode = dplyr::if_else(is_reg, "regression",  "classification"),
        is_reg = is_reg,
        is_multinom = is_multinom,
        is_binary = is_binary
    )
}
