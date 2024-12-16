
#' Prepare Text for Analysis
#' 
#' This function processes a given text string by converting it to lowercase, removing numbers, 
#' non-alphanumeric characters, extra whitespace, and stopwords based on a specified language. 
#' It also transliterates text to ASCII, splits words, and reconstructs a clean text string suitable for analysis.
#'
#' @rdname prepare_text
#' @keywords strings
#' @param text A character vector or object that can be coerced to a character string. Represents the input text to be cleaned.
#' @param stopwords A character vector specifying stopwords removal. Defaults to `"spanish"` stopwords from the tm:stopwords package.
#'
#' @return A cleaned character string, with stopwords removed and text formatted for analysis.
#' @examples
#' # Example usage:
#' prepare_text("¡Hola! Esto es una prueba 123.", lang = "spanish")
#' prepare_text("Hello, World! This is a test 123.", lang = "english")
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
    paste(collapse = " ")  |> 
    print()
}

#' Fuzzy Token Set Ratio
#'
#' This function computes a fuzzy similarity score between two strings based on
#' the token set ratio methodology. It considers the intersection and
#' differences between tokenized word sets from the input strings, and
#' calculates a similarity score normalized by string lengths.
#'
#' @rdname fuzzy_token_set_ratio
#' @keywords strings
#' @param s1 A character string. The first string to compare.
#' @param s2 A character string. The second string to compare.
#' @param score_cutoff A numeric value (default is `0`) specifying the minimum similarity score threshold. 
#' Scores below this threshold may trigger early exits in the computation.
#'
#' @return A numeric similarity score between `0` and `100`, representing the degree of similarity between the two input strings.
#'
#' @details 
#' This function performs the following steps:
#' - Tokenizes the input strings using the `split_text` function.
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
#' @seealso \code{\link{split_text}}, \code{\link{calculate_diff_and_lengths}}, \code{\link{norm_distance}}
#'
#' @importFrom stringdist stringdist
#' @export
fuzzy_token_set_ratio <- function(s1, s2, score_cutoff = 0) {
  if (length(s1) == 0 || length(s2) == 0 || s1 == "" || s2 == "") return(0)  
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
  if (total_len == 0) return(100)
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
  r$sect_ab_dist = (r$intersect_len > 0) + r$diff_ab_len
  r$sect_ba_dist = (r$intersect_len > 0) + r$diff_ba_len
  r
}
