# Tune a recipe using glmnet and lightgbm and stacks

Tune a recipe using glmnet and lightgbm and stacks

## Usage

``` r
score_recipe(rec, resamples, grids = list(10, 10), metric = "accuracy")
```

## Arguments

- rec:

  recipe

- resamples:

  rset

- grids:

  for glmnet and lightgbm tuning

- metric:

  to be compared
