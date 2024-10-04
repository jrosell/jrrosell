#' Do the last fit and get the metrics
#'
#' @keywords tidymodels
#' @rdname last_fit_metrics
#' @param res Tune results
#' @param split The initial split object
#' @param metric What metric to use to select the best workflow
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
#'   step_integer(all_nominal_predictors()) |>
#'   workflow_boost_tree()
#' res <- wf |>
#'   tune::tune_grid(
#'     resamples = folds,
#'     grid = 2,
#'     metrics = metric_set(roc_auc),
#'     control = tune::control_grid(save_workflow = TRUE, verbose = FALSE)
#'   )
#' res |> collect_metrics()
#' res |> last_fit_metrics(split, "roc_auc")
#' best <- res |> fit_best()
#' best |>
#'   augment(testing(split)) |>
#'   roc_auc(case, .pred_Test) |>
#'   pull(.estimate)
#'
#' @export
last_fit_metrics <- function(res, split, metric) {
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
#' @examples
#' recipes::recipe(spray ~ ., data = InsectSprays) |>
#'   prep_juice()
#' recipes::recipe(spray ~ ., data = InsectSprays) |>
#'   workflows::workflow(parsnip::linear_reg()) |>
#'   prep_juice()
#' @source <https://recipes.tidymodels.org/reference/update.step.html>
#' @export
prep_juice <- function(object) {
  if (!requireNamespace("recipes", quietly = TRUE)) stop("recipes package is required")
  if (!requireNamespace("tibble", quietly = TRUE)) stop("tibble package is required")
  if (inherits(object, "workflow")) {
    object <- object |> workflows::extract_preprocessor()
  }
  object |>
    recipes::prep() |>
    recipes::juice() |>
    tibble::glimpse()
}

#' Prep, juice and get cols from a recipe or workflow
#'
#' @keywords tidymodels
#' @rdname prep_juice_ncol
#' @param object A recipe or a workflow object with a recipe
#' @examples
#' recipes::recipe(spray ~ ., data = InsectSprays) |>
#'   prep_juice_ncol()
#' @export
prep_juice_ncol <- function(object) {
  if (!requireNamespace("recipes", quietly = TRUE)) stop("recipes package is required")
  if (!requireNamespace("tibble", quietly = TRUE)) stop("tibble package is required")
  if (inherits(object, "workflow")) {
    object <- object |> workflows::extract_preprocessor()
  }
  object |>
    recipes::prep() |>
    recipes::juice() |>
    ncol()
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
#' @examples
#' recipes::recipe(spray ~ ., data = InsectSprays) |>
#'   recipes::step_ns(count, deg_free = hardhat::tune(), id = "ns") |>
#'   update_step("ns", deg_free = 1)
#' @export
update_step <- function(object, target_id, ...) {
  if (!requireNamespace("recipes", quietly = TRUE)) stop("recipes package is required")
  if (inherits(object, "workflow")) {
    object <- object |> workflows::extract_preprocessor()
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
#' @param engine xgboost, lightgbm (xgboost by default)
#' @param counts Optional logic argument wether mtry use counts or not
#' @param ... optional engine arguments
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
#'   step_integer(all_nominal_predictors()) |>
#'   workflow_boost_tree()
#' doFuture::registerDoFuture()
#' plan(sequential)
#' res <- wf |>
#'   tune::tune_grid(
#'     folds,
#'     grid = 2,
#'     metrics = metric_set(roc_auc),
#'     control = tune::control_grid(save_workflow = TRUE, verbose = FALSE)
#'   )
#' res |> collect_metrics()
#' res |> last_fit_metrics(split, "roc_auc")
#' best <- res |> fit_best()
#' best |>
#'   augment(testing(split)) |>
#'   roc_auc(case, .pred_Test) |>
#'   pull(.estimate)
#' @export
workflow_boost_tree <- function(rec, engine = "xgboost", counts = TRUE, ...) {
  recipe_mode <- .get_recipe_mode(rec)
  mode <- recipe_mode$mode
  rec |>
    workflows::workflow(
      parsnip::boost_tree(mode = mode, engine = engine) |>
        parsnip::set_args(
          trees = parsnip::tune(), learn_rate = parsnip::tune(), # steps and step size
          tree_depth = parsnip::tune(), min_n = parsnip::tune(), loss_reduction = parsnip::tune(), # complexity
          sample_size = parsnip::tune(), mtry = parsnip::tune(), # randomness
          counts = !!counts # , ...
        )
    )
}


#' Create a tuneable glmnet worfklow for regression and classification
#'
#' @keywords tidymodels
#' @rdname workflow_elasticnet
#' @param rec prerocessing recipe to build the workflow
#' @param engine glmnet, spark, brulee (glmnet by default)
#' @param ... Optional engine arguments
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
#'   step_integer(all_nominal_predictors()) |>
#'   workflow_elasticnet()
#' doFuture::registerDoFuture()
#' plan(sequential)
#' res <- wf |>
#'   tune::tune_grid(
#'     folds,
#'     grid = 2,
#'     metrics = metric_set(roc_auc),
#'     control = tune::control_grid(save_workflow = TRUE, verbose = FALSE)
#'   )
#' res |> collect_metrics()
#' res |> last_fit_metrics(split, "roc_auc")
#' best <- res |> fit_best()
#' best |>
#'   augment(testing(split)) |>
#'   roc_auc(case, .pred_Test) |>
#'   pull(.estimate)
#' @export
workflow_elasticnet <- function(rec, engine = "glmnet", ...) {
  recipe_mode <- .get_recipe_mode(rec)
  if (recipe_mode$is_reg) {
    print("regression")
    model <- parsnip::linear_reg("regression", engine) |>
      parsnip::set_args(
        penalty = parsnip::tune(),
        mixture = parsnip::tune() # , ...
      )
  }
  if (recipe_mode$is_binary) {
    print("logistic_reg binary classification")
    model <- parsnip::logistic_reg("classification", engine) |>
      parsnip::set_args(
        penalty = parsnip::tune(),
        mixture = parsnip::tune() # , ...
      )
  }
  if (recipe_mode$is_multinom) {
    print("multinom_reg classification")
    model <- parsnip::multinom_reg("classification", engine) |>
      parsnip::set_args(
        penalty = parsnip::tune(),
        mixture = parsnip::tune() # , ...
      )
  }
  return(workflows::workflow(rec, model))
}

#' Help infer things about the mode and the model to fit
#' @noRd
.get_recipe_mode <- function(rec) {
  outcome <- rec |>
    summary() |>
    dplyr::filter(.data$role == "outcome") |>
    dplyr::pull(.data$variable)
  outcome_vec <- rec$template[[outcome]]
  n_outcome <- dplyr::n_distinct(outcome_vec)
  is_reg <- class(outcome_vec) == "integer" | class(outcome_vec) == "double"
  is_multinom <- class(outcome_vec) == "factor" && n_outcome > 2
  is_binary <- class(outcome_vec) == "factor" && n_outcome == 2
  list(
    mode = dplyr::if_else(is_reg, "regression", "classification"),
    is_reg = is_reg,
    is_multinom = is_multinom,
    is_binary = is_binary
  )
}


#' Fit a workflow with specific parameters
#'
#' @keywords tidymodels
#' @rdname fit_params
#' @param wf workflow
#' @param resamples rset
#' @param param_info for tune_* functions
#' @param grid for tune_* functions
#' @param fn the name of the function to run when tuning
#' @param ... Optional engine arguments
#' @examples
#' library(tidymodels)
#' library(xgboost)
#' library(modeldata)
#' data(cells)
#' split <- cells |>
#'   mutate(across(where(is.character), as.factor)) |>
#'   sample_n(500) |>
#'   initial_split(strata = case)
#' train <- training(split)
#' resamples <- vfold_cv(train, v = 2, strata = case)
#' wf_spec <- train |>
#'   recipe(case ~ .) |>
#'   step_integer(all_nominal_predictors()) |>
#'   workflow(boost_tree(mode = "classification"))
#' res_spec <- wf_spec |> fit_results(resamples)
#' res_spec |> collect_metrics()
#' @export
fit_results <- function(wf, resamples, param_info = NULL, grid = 10, fn = "tune_grid", ...) {
  if (nrow(tune::tune_args(wf)) == 0) {
    fn <- "fit_resamples"
  }
  wfset <- workflowsets::as_workflow_set(wf = wf) |>
    workflowsets::workflow_map(
      resamples = resamples, param_info = param_info, grid = grid, fn = fn, ...
    )
  wfset$result[[1]]
}




#' Tune a recipe using glmnet and lightgbm and stacks
#'
#' @keywords tidymodels
#' @rdname score_recipe
#' @param rec recipe
#' @param resamples rset
#' @param grids for glmnet and lightgbm tuning
#' @param metric to be compared

score_recipe <- function(rec,
                         resamples,
                         grids = list(10, 10),
                         metric = "accuracy") {
  glmnet_res <-
    rec |>
    workflow_elasticnet() |>
    tune::tune_grid(
      resamples = resamples,
      grid = grids[[1]],
      control = tune::control_grid(save_pred = TRUE, save_workflow = TRUE)
    )

  lgbm_res <-
    rec |>
    workflow_boost_tree() |>
    tune::tune_grid(
      resamples = resamples,
      grid = grids[[2]],
      control = tune::control_grid(save_pred = TRUE, save_workflow = TRUE)
    )

  data_stack <- stacks::stacks() |>
    stacks::add_candidates(glmnet_res) |>
    stacks::add_candidates(lgbm_res)

  model_stack <- data_stack |>
    stacks::blend_predictions() |>
    stacks::fit_members()

  glmnet_metrics <- glmnet_res |>
    tune::collect_metrics() |>
    dplyr::filter(.data$.metric == metric)
  lgbm_metrics <- lgbm_res |>
    tune::collect_metrics() |>
    dplyr::filter(.data$.metric == metric)
  stack_metrics <- model_stack$metrics |>
    dplyr::filter(.data$.metric == metric)

  list(
    glmnet_res = glmnet_res,
    glmnet_metrics = glmnet_metrics,
    lgbm_res = lgbm_res,
    lgbm_metrics = lgbm_metrics,
    stack = model_stack,
    stack_metrics = stack_metrics
  )
}
