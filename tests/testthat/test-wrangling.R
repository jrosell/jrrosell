test_that("summarize_n_distinct works", {
  library(jrrosell)
  library(tibble)
  library(dplyr)
  df <- tibble(
    a = c(1, 1, 1),
    b = c(2, 2, 2),
    c = c(1, 2, 3),
    d = c("A", "A", "A")
  )

  # Find constant columns
  result <- df |>
    dplyr::summarise(
      dplyr::across(dplyr::everything(), dplyr::n_distinct)
    )

  expect_equal(result, summarize_n_distinct(df))
})

# test_that("sum_missing works", {
#   library(jrrosell)
#   library(tibble)
#   library(dplyr)
#   df <- tibble(
#     a = c(1, NA, 1),
#     b = c(2, 2, 2),
#     c = c(1, 2, 3),
#     d = c("A", NA, "A")
#   )
#   n_missing <- sum(colSums(is.na(df)))
#   sum_missing(df)
# })


test_that("duplicate_rows works", {
  library(jrrosell)
  library(tibble)
  library(dplyr)
  df <- tibble(
    a = c(1, 2, 2, 4),
    b = c("x", "y", "y", "z"),
    c = c(10, 20, 20, 40)
  )
  total_rows <- nrow(df)
  unique_rows <- df %>%
    distinct() %>%
    nrow()
  n_duplicated <- total_rows - unique_rows
  expect_equal(n_duplicated, count_duplicated_rows(df))
})


# test_that("plot_missing works", {
#   library(jrrosell)
#   library(tibble)
#   library(dplyr)
#   df <- tibble(
#     a = c(1, NA, 1),
#     b = c(2, 2, 2),
#     c = c(1, 2, 3),
#     d = c("A", NA, "A")
#   )
#   p <- plot_missing(df)
#   print(p)
# })


# test_that("plot_histograms works", {
#   library(jrrosell)
#   library(tibble)
#   library(dplyr)
#   df <- tibble(
#     a = c(1, 2, 2, 4, 3),
#     b = c("x", "y", "y", "z", "z"),
#     c = c(10, 20, 20, 40, 50),
#     d = c("a", "b", "b", "b", "b"),
#     e = c("a", "b", "c", "d", "e"),
#   )
#   p <- plot_histograms(df)
#   print(p)
# })

# test_that("plot_bars works", {
#   library(jrrosell)
#   library(tibble)
#   library(dplyr)
#   df <- tibble(
#     a = c(1, 2, 2, 4, 3),
#     b = c("x", "y", "y", "z", "z"),
#     c = c(10, 20, 20, 40, 50),
#     d = c("a", "b", "b", "b", "b"),
#     e = c("a", "b", "c", "d", "e"),
#   )
#   p <- plot_bars(df)
#   print(p)
# })


# test_that("glimpses works", {
#   library(jrrosell)
#   library(tibble)
#   library(dplyr)
#   df1 <- tibble(
#     a = c(1, 2, 2, 4, 3),
#     b = c("x", "y", "y", "z", "z"),
#     c = c(10, 20, 20, 40, 50),
#     d = c("a", "b", "b", "b", "b"),
#     e = c("a", "b", "c", "d", "e"),
#   )
#   df2 <- tibble(
#     a = c(1, 2, 2, 4, 3),
#     b = c("x", "y", "y", "z", "z"),
#     c = c(10, 20, 20, 40, 50),
#     d = c("a", "b", "b", "b", "b")
#   )
#   glimpses(df1, df2)
# })


test_that("count_sorted works", {
  library(jrrosell)
  library(tibble)
  library(dplyr)
  df <- tibble(
    a = c(1, 2, 2, 4, 3),
    b = c("x", "y", "y", "z", "z"),
    c = c(10, 20, 20, 40, 50),
    d = c("a", "b", "b", "b", "b")
  )
  result <- count_sorted(df, d)
  expect_true(inherits(result, "data.frame"))
  expect_true(nrow(result) == 2)
  expect_true(sum(result$n) == 5)
})

# test_that("plot_outcome works", {
#   library(jrrosell)
#   library(tibble)
#   library(dplyr)
#   library(ggplot2)
#   df <- tibble(
#     a = c(1, 2, 2, 4, 3),
#     b = c("x", "y", "y", "z", "z"),
#     c = c(10, 20, 20, 40, 50),
#     d = c("a", "b", "b", "b", "b")
#   )
#   p_nominal <- plot_outcome(df, d)
#   print(p_nominal)
#   p_numeric <- plot_outcome(df, a)
#   print(p_numeric)

# })
