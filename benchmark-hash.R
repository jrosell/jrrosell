if (!requireNamespace("rlang")) stop("install.packages('rlang')")
if (!requireNamespace("pak")) stop("install.packages('pak')")
pkgs <- rlang::chr(
  "tidyverse",      
  "uwu",
  "digest", 
  "openssl",
  "testthat",
)
rlang::check_installed(pkgs)
purrr::walk(pkgs, library, character.only = TRUE, quietly = TRUE)

if (!file.exists("sorted_df.rds")) {
  size <- 1e5
  df <- tibble(    
    id7 = uwu::new_v7(size),
    chr = sample(letters, size, replace = TRUE),
    int = sample(size),
    dbl = sample(size)*10/3,
    lgl = sample(c(TRUE, FALSE), size, replace = TRUE)
  )
  sorted_df <- df[, sort(names(df)), drop = FALSE]
  write_rds(sorted_df, "sorted_df.rds")
}
sorted_df <- read_rds("sorted_df.rds")


rlang_hash <- \(sorted_df, primary_keys) {
  sorted_df[["row_hash"]] <- 
    sorted_df |> 
    group_nest({{ primary_keys }}) |> 
    mutate(row = map_chr(data, \(x) paste0(x, collapse = "|"))) |> 
    mutate(row_hash = map_chr(row, \(x) rlang::hash(x))) |>     
    pull(row_hash)

  sorted_df
}

digest_hash <- \(sorted_df, primary_keys, algo = "xxh3_128") {
  sorted_df[["row_hash"]] <- 
    sorted_df |> 
    group_nest({{ primary_keys }}) |> 
    mutate(row = map_chr(data, \(x) paste0(x, collapse = "|"))) |> 
    mutate(row_hash = map_chr(row, \(x) digest::digest(x, algo=algo, serialize = FALSE, ascii = FALSE, raw = FALSE))) |>     
    pull(row_hash)

  sorted_df
}

digest_hash_raw <- \(sorted_df, primary_keys, algo = "xxh3_128") {
  sorted_df[["row_hash"]] <- 
    sorted_df |> 
    group_nest({{ primary_keys }}) |> 
    mutate(row = map_chr(data, \(x) paste0(x, collapse = "|"))) |> 
    mutate(row_hash = map_chr(row, \(x) digest::digest(x, algo=algo, serialize = FALSE, ascii = FALSE, raw = TRUE))) |>     
    pull(row_hash)

  sorted_df
}

digest_hash_serialize_raw <- \(sorted_df, primary_keys, algo = "xxh3_128", raw = FALSE) {
  sorted_df[["row_hash"]] <- 
    sorted_df |> 
    group_nest({{ primary_keys }}) |> 
    mutate(row = map_chr(data, \(x) paste0(x, collapse = "|"))) |> 
    mutate(row_hash = map_chr(row, \(x) digest::digest(x, algo=algo, serialize = TRUE, ascii = FALSE, raw = TRUE))) |>     
    pull(row_hash)

  sorted_df
}

sha1_hash <- \(sorted_df, primary_keys) {
  sorted_df[["row_hash"]] <- 
    sorted_df |> 
    group_nest({{ primary_keys }}) |> 
    mutate(row = map_chr(data, \(x) paste0(x, collapse = "|"))) |> 
    mutate(row_hash = map_chr(row, \(x) openssl::sha1(x))) |>     
    pull(row_hash)

  sorted_df
}
md5_hash <- \(sorted_df, primary_keys) {
  sorted_df[["row_hash"]] <- 
    sorted_df |> 
    group_nest({{ primary_keys }}) |> 
    mutate(row = map_chr(data, \(x) paste0(x, collapse = "|"))) |> 
    mutate(row_hash = map_chr(row, \(x) openssl::md5(x))) |>     
    pull(row_hash)

  sorted_df
}

results <- bench::mark(
  rlang_hash(sorted_df, primary_keys = id7) |> head(),
  digest_hash(sorted_df, primary_keys = id7, algo = "xxh3_128") |> head(),
  digest_hash_raw(sorted_df, primary_keys = id7, algo = "xxh3_128") |> head(),
  digest_hash_serialize_raw(sorted_df, primary_keys = id7, algo = "xxh3_128") |> head(),
  sha1_hash(sorted_df, primary_keys = id7) |> head(),
  md5_hash(sorted_df, primary_keys = id7) |> head(),
  check = FALSE,
  filter_gc = FALSE,
  memory = FALSE,
  iterations = 10, 
  relative = TRUE
)
results[["time_itr"]] <- results[["total_time"]] / results[["n_itr"]] 
results[order(results[["median"]]), c("expression", "median", "time_itr")]

sessioninfo::session_info()



devtools::load_all()
df <- data.frame(    
   id = c(1, 2, 3),
   name = c("AAAAA", "BBBB", "CCC")
)
df_hashed <- add_row_hash(df, "id")

df_hashed |> readr::write_rds("benchmark-hash.rds")
df_hashed_rds <- readr::read_rds("benchmark-hash.rds")

df_hashed |> arrow::write_parquet("benchmark-hash.parquet")
df_hashed_parquet <- arrow::read_parquet("benchmark-hash.parquet")

expect_identical(
  df_hashed_rds |> as_tibble(),
  df_hashed_parquet |> as_tibble()
)
