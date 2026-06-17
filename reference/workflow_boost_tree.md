# Create an xgboost tunable workflow for regression and classification

Create an xgboost tunable workflow for regression and classification

## Usage

``` r
workflow_boost_tree(rec, engine = "xgboost", counts = TRUE, ...)
```

## Arguments

- rec:

  prerocessing recipe to build the workflow

- engine:

  xgboost, lightgbm (xgboost by default)

- counts:

  Optional logic argument wether mtry use counts or not

- ...:

  optional engine arguments
