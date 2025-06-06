#' Prepare Text for Analysis
#'
#' This function processes a given text string by converting it to lowercase, removing numbers,
#' non-alphanumeric characters, extra whitespace, and stopwords based on a specified language.
#' It also transliterates text to ASCII, splits words, and reconstructs a clean text string suitable for analysis.
#'
#' @rdname prepare_text
#' @keywords text
#' @param text A character vector or object that can be coerced to a character string. Represents the input text to be cleaned.
#' @param stopwords A character vector specifying stopwords removal. Defaults to `"spanish"` stopwords from the tm:stopwords package.
#'
#' @return A cleaned character string, with stopwords removed and text formatted for analysis.
#' @examples
#' # Example usage:
#' prepare_text("¡Hola! Esto es una prueba 123.")
#'
#' @importFrom purrr keep
#' @importFrom tm stopwords
#' @export
prepare_text <- function(text, stopwords = NULL) {
  if (is.null(stopwords)) {
    stopwords <- tm::stopwords("spanish")
  }
  text |>
    as.character() |>
    tolower() |>
    gsub("\\d+", "", x = _) |>
    gsub("[^a-zA-Z0-9\\s]", " ", x = _) |>
    gsub("\\s+", " ", x = _) |>
    trimws() |>
    iconv(to = "ASCII//TRANSLIT") |>
    strsplit("\\s+") |>
    unlist() |>
    purrr::keep(\(words) !words %in% stopwords) |>
    paste(collapse = " ")
}

#' Fuzzy Token Set Ratio
#'
#' This function computes a fuzzy similarity score between two strings based on
#' the token set ratio methodology. It considers the intersection and
#' differences between tokenized word sets from the input strings, and
#' calculates a similarity score normalized by string lengths.
#'
#' @rdname fuzzy_token_set_ratio
#' @keywords text
#' @param s1 A character string. The first string to compare.
#' @param s2 A character string. The second string to compare.
#' @param score_cutoff A numeric value (default is `0`) specifying the minimum similarity score threshold.
#' Scores below this threshold may trigger early exits in the computation.
#'
#' @return A numeric similarity score between `0` and `100`, representing the degree of similarity between the two input strings.
#'
#' @details
#' This function performs the following steps:
#' - Tokenizes the input strings.
#' - Identifies intersecting and differing tokens between the two tokenized sets.
#' - Computes the longest common subsequence (LCS) distance for differing tokens and normalizes it.
#' - Calculates similarity ratios for intersecting tokens combined with differing token sets.
#' - Returns the maximum of the normalized LCS distance and the two intersecting token ratios.
#'
#' The function short-circuits to return `100` if one token set is a subset of the other. If either input string is empty, the function returns `0`.
#'
#' @examples
#' # Example usage:
#' fuzzy_token_set_ratio("fuzzy was a bear", "fuzzy was a dog", score_cutoff = 80)
#' fuzzy_token_set_ratio("hello world", "world hello")
#'

#'
#' @importFrom stringdist stringdist
#' @export
fuzzy_token_set_ratio <- function(s1, s2, score_cutoff = 0) {
  if (length(s1) == 0 || length(s2) == 0 || s1 == "" || s2 == "") {
    return(0)
  }
  tokens_a <- fuzzy_split_text(s1)
  tokens_b <- fuzzy_split_text(s2)
  r <- fuzzy_calculate_diff_and_lengths(tokens_a, tokens_b)
  if (r$intersect_len > 0 && (r$diff_ab_len == 0 || r$diff_ba_len == 0)) {
    return(100)
  }
  lcs_distance <- stringdist::stringdist(r$diff_ab_joined, r$diff_ba_joined, method = "lcs")
  normalized_indel_distance <- fuzzy_norm_distance(
    lcs_distance, r$sect_ab_len, r$sect_ba_len, score_cutoff
  )
  if (r$intersect_len == 0) {
    return(normalized_indel_distance)
  }
  sect_ab_ratio <- fuzzy_norm_distance(r$sect_ab_dist, r$intersect_len, r$sect_ab_len, score_cutoff)
  sect_ba_ratio <- fuzzy_norm_distance(r$sect_ba_dist, r$intersect_len, r$sect_ba_len, score_cutoff)
  return(max(normalized_indel_distance, sect_ab_ratio, sect_ba_ratio))
}


#' @noRd
fuzzy_split_text <- function(sequence) {
  words <- strsplit(sequence, "\\s+")[[1]]
  words <- words[words != ""]
  return(words)
}

#' @noRd
fuzzy_norm_distance <- function(dist, len1, len2, score_cutoff = 0) {
  total_len <- len1 + len2
  if (total_len == 0) {
    return(100)
  }
  norm_sim <- 1 - dist / total_len
  if (norm_sim < score_cutoff / 100) {
    return(0)
  }
  return(norm_sim * 100)
}

#' @noRd
fuzzy_calculate_diff_and_lengths <- function(tokens_a, tokens_b) {
  r <- list()
  r$diff_ab <- setdiff(tokens_a, tokens_b)
  r$diff_ba <- setdiff(tokens_b, tokens_a)
  r$intersect_len <- sort(intersect(tokens_a, tokens_b)) |>
    paste(collapse = " ") |>
    nchar()
  r$diff_ab_len <- nchar(paste(sort(r$diff_ab), collapse = " "))
  r$diff_ba_len <- nchar(paste(sort(r$diff_ba), collapse = " "))
  r$diff_ab_joined <- paste(sort(r$diff_ab), collapse = " ")
  r$diff_ba_joined <- paste(sort(r$diff_ba), collapse = " ")
  r$sect_ab_len <- r$intersect_len + (r$intersect_len > 0) + r$diff_ab_len
  r$sect_ba_len <- r$intersect_len + (r$intersect_len > 0) + r$diff_ba_len
  r$sect_ab_dist <- (r$intersect_len > 0) + r$diff_ab_len
  r$sect_ba_dist <- (r$intersect_len > 0) + r$diff_ba_len
  r
}



#' Create a vector of characters from a string
#'
#' @rdname chars
#' @keywords text
#' @param x a vector of characters of length 1.
#' @param ... unused
#' @details
#' `chars` expects a single string as input. To create a list of these,
#' consider `lapply(strings, chars)`.
#' @return a vector of characters
#' @examples
#' chars("hola")
#' @seealso [https://github.com/jonocarroll/charcuterie](https://github.com/jonocarroll/charcuterie)
#' @export
chars <- function(x, ...) {
  stopifnot("chars expects a single input; try sapply(x, chars)" = length(x) == 1)
  strsplit(x, "")[[1]]
}




#' Get a sentiments by language
#'
#' The multilingual sentiment lexicon was obtained from here on 2024-12-18 https://aclanthology.org/P14-2063/
#'
#' @rdname get_sentiments_by_language
#' @keywords text
#' @param language two letters language code.
#' @param lexicon default and only valid value "chen_skiena"
#' @return A tibble with word and sentiment columns
#' @examples
#' get_sentiments_by_language("ca")
#' @details
#' The files were generated this way:
#' chen_skiena_lexicon <-
#'    bind_rows(
#'      here::here("P14-2063.Datasets", "readable_neg_words_list.txt") |>
#'      read_delim(delim = " ", col_names = c("word", "lang")) |>
#'      mutate(sentiment = factor("negative", levels = c("negative", "positive"))),
#'      here::here("P14-2063.Datasets", "readable_pos_words_list.txt") |>
#'      read_delim(delim = " ", col_names = c("word", "lang")) |>
#'      mutate(sentiment = factor("positive", levels = c("negative", "positive")))
#'    )
#'  chen_skiena_lexicon |>
#'    write_fst(
#'      here::here("inst", "extdata", "chen_skiena_lexicon.fst"),
#'      compress = 100
#'    )
#' top_languages <- rlang::chr(
#' ca = 'catalan',
#' zh = 'chinese_simplified',
#' da = 'danish',
#' nl = 'dutch',
#' en = 'english',
#' eo = 'esperanto',
#' fi = 'finnish',
#' fr = 'french',
#' de = 'german',
#' el = 'greek',
#' hu = 'hungarian',
#' it = 'italian',
#' la = 'latin',
#' pt = 'portuguese',
#' es = 'spanish',
#' sv = 'swedish'
#' )
#' nrc_lexicon <- read_delim("NRC-Emotion-Lexicon-ForVariousLanguages.txt", delim = "\\t") |>
#' janitor::clean_names() |>
#' pivot_longer(cols = anger:trust, names_to = "sentiment") |>
#' rename(english = english_word) |>
#' pivot_longer(cols = -c(sentiment, value), names_to = "language", values_to = "word") |>
#' filter(sentiment %in% c("positive", "negative")) |>
#' filter(language %in% top_languages) |>
#' transmute(
#'     sentiment = factor(sentiment, c("negative", "positive")),
#'     language = factor(language, unique(language)),
#'     word = factor(word, unique(word)),
#'   ) |>
#'   glimpse()
#' nrc_lexicon |>
#'   write_fst(
#'     here::here("nrc_lexicon.fst"),
#'     compress = 100
#'   )
#' @seealso [https://juliasilge.github.io/tidytext/reference/get_sentiments.html](https://juliasilge.github.io/tidytext/reference/get_sentiments.html)
#' @export
get_sentiments_by_language <- \(language = "en", lexicon = "chen_skiena") {
  if (!requireNamespace("fst")) {
    stop("The package `fst` is required for this functionality")
  }
  read_fst <- utils::getFromNamespace("read_fst", "fst")
  if (!lexicon %in% c("chen_skiena", "nrc")) {
    stop("Use a supported lexicon")
  }
  if (lexicon == "chen_skiena") {
    file_path <- system.file("extdata", "chen_skiena_lexicon.fst", package = "jrrosell")
    if (!file.exists(file_path)) {
      stop("Lexicon file not found.")
    }
    return(read_fst(file_path) |>
      dplyr::filter(.data[["lang"]] == language) |>
      dplyr::select(-dplyr::all_of("lang")))
  }
  if (lexicon == "nrc") {
    file_path <- system.file("extdata", "nrc_lexicon.fst", package = "jrrosell")
    if (!file.exists(file_path)) {
      stop("Lexicon file not found.")
    }
    return(read_fst(file_path) |>
      dplyr::filter(.data[["lang"]] == language) |>
      dplyr::select(-dplyr::all_of("lang")))
  }
}
