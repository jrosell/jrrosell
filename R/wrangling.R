#' Select constant columns from a data.frame
#'
#' @rdname summarize_n_distinct
#' @keywords wrangling
#' @param df a data.frame
#' @examples
#' summarize_n_distinct(data.frame(a = c(1, 2), b = c(2, 3)))
#' summarize_n_distinct(data.frame(a = c(1, 1), b = c(2, 3)))
#' @export
summarize_n_distinct <- function(df) {
  df |> dplyr::summarise(
    dplyr::across(dplyr::everything(), dplyr::n_distinct)
  )
}

#' Sum the missing values from a data.frame
#'
#' @rdname sum_missing
#' @keywords wrangling
#' @param ... one or multiple data.frame
#' @examples
#' sum_missing(data.frame(a = c(1, 2), b = c(3, 4)))
#' sum_missing(data.frame(a = c(1, NA), b = c(3, 4)))
#' sum_missing(data.frame(a = c(1, NA), b = c(NA, 4)))
#' sum_missing(data.frame(a = c(NA, NA), b = c(NA, NA)))
#' @export
sum_missing <- function(...) {
  dfs <- list(...)
  summarize_df <- function(df, df_name) {
    n_missing <- sum(colSums(is.na(df)))
    summary_string <- paste(
      df_name, "has", n_missing, "missing values."
    )
    return(summary_string)
  }
  df_names <- as.character(match.call())[-1]
  for (i in seq_along(dfs)) {
    cat(summarize_df(dfs[[i]], df_names[i]), "\n")
  }
}


#' Count the number of duplicated rows
#'
#' @rdname count_duplicated_rows
#' @keywords wrangling
#' @param df a data.frame
#' @examples
#' count_duplicated_rows(data.frame(a = c(1, 2, 3), b = c(3, 4, 5)))
#' count_duplicated_rows(data.frame(a = c(1, 2, 3), b = c(1, 4, 5)))
#' @export
count_duplicated_rows <- function(df) {
  total_rows <- nrow(df)
  unique_rows <- df |>
    dplyr::distinct() |>
    nrow()
  n_duplicates <- total_rows - unique_rows
  n_duplicates
}


#' Glimpse multiple datasets
#'
#' @rdname glimpses
#' @keywords wrangling
#' @param ... Multiple data.frame
#' @examples
#' df1 <- data.frame(a = c(1, 2))
#' df2 <- data.frame(b = c(3, 4))
#' glimpses(df1, df2)
#' @export
glimpses <- function(...) {
  dfs <- list(...)
  summarize_df <- function(df, df_name) {
    # Number of rows and columns
    n_rows <- nrow(df)
    n_cols <- ncol(df)
    distinct_counts <- sapply(df, function(col) length(unique(col)))
    distinct_summary <- paste(
      names(distinct_counts), distinct_counts,
      sep = " ", collapse = ", "
    )
    summary_string <- paste(
      df_name, n_rows, "rows and",
      n_cols, "columns", "with", distinct_summary, "distinct values."
    )
    return(summary_string)
  }
  df_names <- as.character(match.call())[-1]
  for (i in seq_along(dfs)) {
    cat(summarize_df(dfs[[i]], df_names[i]), "\n")
  }
}


#' Plot missing values
#'
#' @rdname plot_missing
#' @keywords wrangling
#' @param df a data.frame
#' @examples
#' plot_missing(data.frame(a = c(1, NA), b = c(NA, 4)))
#' @export
plot_missing <- function(df) {
  missing_data <- df |>
    dplyr::summarise(dplyr::across(dplyr::everything(), \(x) mean(is.na(x)))) |>
    tidyr::pivot_longer(dplyr::everything(), names_to = "column", values_to = "missing_pct") |>
    dplyr::arrange(.data$missing_pct) # Order by percentage of missing values

  ggplot2::ggplot(missing_data, ggplot2::aes(x = stats::reorder(.data$column, .data$missing_pct), y = .data$missing_pct)) +
    ggplot2::geom_bar(stat = "identity", fill = "steelblue") +
    ggplot2::labs(x = "", y = "Percentage Missing") +
    ggplot2::theme_minimal() +
    ggplot2::coord_flip() +
    ggplot2::theme(
      axis.text.y = ggplot2::element_text(size = 8)
    ) +
    ggplot2::scale_y_continuous(
      labels = scales::percent_format(),
      limits = c(0, 1)
    )
}


#' Plot histograms for double columns
#'
#' @rdname plot_histograms
#' @keywords wrangling
#' @param df a data.frame
#' @param ... optional parameters to geom_histogram
#' @examples
#' plot_histograms(data.frame(a = c(1, 2), b = c(1, 3)))
#' @export
plot_histograms <- function(df, ...) {
  df |>
    dplyr::select(dplyr::where(is.double)) |>
    dplyr::mutate(rows = dplyr::row_number()) |>
    tidyr::pivot_longer(cols = -.data$rows) |>
    dplyr::filter(!is.na(.data$value)) |>
    ggplot2::ggplot(ggplot2::aes(.data$value)) +
    ggplot2::facet_wrap(~name, scales = "free") +
    ggplot2::geom_histogram(...) +
    ggplot2::theme_minimal() +
    ggplot2::labs(x = "", y = "") +
    ggplot2::guides(fill = ggplot2::guide_legend(title = "Histogram")) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 0.1)
    )
}


#' Plot bars for non double columns
#'
#' @rdname plot_bars
#' @keywords wrangling
#' @param df a data.frame
#' @param ... optional parameters to geom_histogram
#' @param top_values fist most common values (default 50)
#' @examples
#' plot_bars(data.frame(a = c("x", "y"), b = c("z", "z")))
#' @export
plot_bars <- function(df, ..., top_values = 50) {
  df |>
    dplyr::select(-dplyr::where(is.double)) |>
    dplyr::mutate(rows = dplyr::row_number()) |>
    tidyr::pivot_longer(cols = -.data$rows) |>
    dplyr::filter(!is.na(.data$value)) |>
    dplyr::group_by(.data$name, .data$value) |>
    dplyr::summarise(count = dplyr::n(), .groups = "drop") |>
    dplyr::arrange(.data$name, dplyr::desc(.data$count)) |>
    dplyr::group_by(.data$name) |>
    dplyr::slice_head(n = top_values) |>
    dplyr::ungroup() |>
    ggplot2::ggplot(ggplot2::aes(.data$value)) +
    ggplot2::facet_wrap(~name, scales = "free") +
    ggplot2::geom_bar(...) +
    ggplot2::theme_minimal() +
    ggplot2::labs(x = "", y = "") +
    ggplot2::guides(fill = ggplot2::guide_legend(title = "Bar")) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 0.1)
    ) +
    ggplot2::coord_flip()
}

#' Count a variable or variables sorted
#'
#' It returns the ordered counts of the variable in the data.frame.
#' @rdname count_sorted
#' @keywords wrangling
#' @param df a data.frame
#' @param ... the variables to use and other arguments to count
#' @examples
#' data.frame(a = c("x", "y", "x"), b = c("z", "z", "n")) |>
#'   count_sorted(a)
#' @export
count_sorted <- function(df, ...) {
  df |>
    dplyr::count(..., sort = TRUE)
}


#' Plot a variable
#'
#' It returns a bar or a histogram of the variable
#' @rdname plot_variable
#' @keywords wrangling
#' @param df a data.frame
#' @param variable the variable to use.
#' @param ... params passed to geom_*
#' @param type numeric (default) or nominal.
#' @examples
#' data.frame(a = c("x", "y"), b = c("z", "z")) |>
#'   plot_variable(a)
#' @export
plot_variable <- function(df, variable, ..., type = "numeric") {
  variable_quo <- rlang::enquo(variable)
  if (!rlang::quo_name(variable_quo) %in% colnames(df)) {
    stop("The outcome variable does not exist in the dataframe.")
  }
  variable_data <- df |> dplyr::pull(!!variable_quo)
  if (is.numeric(variable_data) && type == "numeric") {
    ggplot2::ggplot(df, ggplot2::aes(!!variable_quo)) +
      ggplot2::geom_histogram(binwidth = 1, fill = "steelblue", color = "white", ...) +
      ggplot2::labs(x = rlang::quo_name(variable_quo), y = "Frequency", title = paste("Histogram of", rlang::quo_name(variable_quo))) +
      ggplot2::theme_minimal()
  } else if (is.factor(variable_data) || is.character(variable_data) || type == "nominal") {
    ggplot2::ggplot(df, ggplot2::aes(!!variable_quo)) +
      ggplot2::geom_bar(fill = "steelblue", ...) +
      ggplot2::labs(x = rlang::quo_name(variable_quo), y = "Count", title = paste("Bar Plot of", rlang::quo_name(variable_quo))) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
      ) +
      ggplot2::coord_flip()
  } else {
    stop("Unsupported outcome variable type.")
  }
}
