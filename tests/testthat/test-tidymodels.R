test_that("workflow_boost_tree works", {
  # devtools::load_all()
  library(tidymodels)
  library(xgboost)
  library(modeldata)
  data(cells)
  split <- cells |>
    mutate(across(where(is.character), as.factor)) |>
    sample_n(500) |>
    initial_split(strata = case)
  train <- training(split)
  folds <- vfold_cv(train, v = 2, strata = case)
  wf <- train |>
    recipe(case ~ .) |>
    step_integer(all_nominal_predictors()) |>
    workflow_boost_tree()
  res <- wf |> tune_grid(folds, grid = 2, metrics = metric_set(roc_auc), control = control_grid(save_workflow = TRUE, verbose = FALSE))
  res |> collect_metrics()
  res |> last_fit_metrics(split, "roc_auc")
  best <- res |> fit_best()
  estimate <- best |>
    augment(testing(split)) |>
    roc_auc(case, .pred_Test) %>%
    pull(.estimate)
  print(paste("roc_auc:", estimate))
  expect_true(estimate > 0.2 & estimate < 1)
})


test_that("workflow_elasticnet for logistic_reg works", {
  # devtools::load_all()
  library(tidymodels)
  library(glmnet)
  library(modeldata)
  data(cells)
  split <- cells |>
    mutate(across(where(is.character), as.factor)) |>
    sample_n(500) |>
    initial_split(strata = case)
  train <- training(split)
  folds <- vfold_cv(train, v = 2, strata = case)
  wf <- train |>
    recipe(case ~ .) |>
    step_dummy(all_nominal_predictors()) |>
    workflow_elasticnet()
  res <- wf |> tune_grid(folds, grid = 2, metrics = metric_set(roc_auc), control = control_grid(save_workflow = TRUE, verbose = FALSE))
  res |> collect_metrics()
  res |> last_fit_metrics(split, "roc_auc")
  best <- res |> fit_best()
  estimate <- best |>
    augment(testing(split)) |>
    roc_auc(case, .pred_Test) %>%
    pull(.estimate)
  print(paste("roc_auc:", estimate))
  expect_true(estimate > 0.2 & estimate < 1)
})


test_that("workflow_elasticnet for multinom_reg works", {
  # devtools::load_all()
  library(tidymodels)
  library(glmnet)
  split <- iris |>
    initial_split(strata = Species)
  train <- training(split)
  folds <- bootstraps(train, times = 2)
  wf <- train |>
    recipe(Species ~ .) |>
    workflow_elasticnet()
  res <- wf |> tune_grid(folds, grid = 2, control = control_grid(save_workflow = TRUE, verbose = FALSE))
  res |> collect_metrics()
  res |> last_fit_metrics(split, "accuracy")
  best <- res |> fit_best()
  estimate <- best |>
    augment(testing(split)) |>
    accuracy(Species, .pred_class) %>%
    pull(.estimate)
  print(paste("accuracy:", estimate))
  expect_true(estimate > 0.85 & estimate <= 1)
})


test_that("fit_results works", {
  # devtools::load_all()
  library(tidymodels)
  library(xgboost)
  library(modeldata)
  data(cells)
  split <- cells |>
    mutate(across(where(is.character), as.factor)) |>
    sample_n(500) |>
    initial_split(strata = case)
  train <- training(split)
  resamples <- vfold_cv(train, v = 2, strata = case)
  wf_spec <- train |>
    recipe(case ~ .) |>
    step_integer(all_nominal_predictors()) |>
    workflow(boost_tree(mode = "classification"))
  res_spec <- wf_spec |> fit_results(resamples)
  metrics_spec <- res_spec |> collect_metrics()
  wf_tune <- train |>
    recipe(case ~ .) |>
    step_integer(all_nominal_predictors()) |>
    workflow_boost_tree()
  res_tune <- wf_tune |> fit_results(resamples, grid = 2)
  metrics_tune <- res_tune |> collect_metrics()
  expect_s3_class(res_spec, "tune_results")
  expect_s3_class(res_tune, "tune_results")
  expect_true("mean" %in% names(metrics_spec))
  expect_true("mean" %in% names(metrics_tune))
})



test_that("score_recipe works", {
  # devtools::load_all()
  library(tidymodels)
  library(xgboost)
  library(modeldata)
  library(stacks)
  data(cells)
  split <- cells |>
    mutate(across(where(is.character), as.factor)) |>
    sample_n(500) |>
    initial_split(strata = case)
  train <- training(split)
  resamples <- vfold_cv(train, v = 2, strata = case)
  rec <- train |>
    recipe(case ~ .) |>
    step_normalize(all_numeric()) |>
    step_dummy(all_nominal_predictors())
  res <- rec |>
    score_recipe(resamples, grids = list(2, 2))

  res_df <- data.frame(
    model = c("stack", "glmnet", "lgbm"),
    result = c(
      max(res$stack_metrics[1, ]$mean),
      max(res$lgbm_metrics[1, ]$mean),
      max(res$glmnet_metrics[1, ]$mean)
    )
  )
  print(res_df)
  expect_true(res_df |> nrow() == 3)
  expect_true(res_df |> pull(result) |> max() > 0.4)
})
