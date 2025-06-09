#' Make a sound and send an email when a process finished
#'
#' The notify_finished make a sound using beepr::beep, compose and email and send it returing the blastula::smtp_send call results.
#'
#' @rdname notify_finished
#' @keywords processing
#' @param name The process name (Required)
#' @param body The contents of the email (Default "")
#' @param ...  Additional arguments to pass to the template function. If you're using the default template, you can use font_family to control the base font, and content_width to control the width of the main content; see blastula_template(). By default, the content_width is set to ⁠1000px⁠. Using widths less than ⁠600px⁠ is generally not advised but, if necessary, be sure to test such HTML emails with a wide range of email clients before sending to the intended recipients. The Outlook mail client (Windows, Desktop) does not respect content_width.
#' @param sound The sound for beepr::beep call (Default 1)
#' @param tictoc_result the result from tictoc::toc (Default NULL)
#'
#' @details The following environment variables should be set:
#' * MY_SMTP_USER from
#' * MY_SMTP_RECIPIENT to
#' * MY_SMTP_PASSWORD service password (for gmail you can use https://myaccount.google.com/apppasswords)
#' * MY_SMTP_PROVIDER blastula provider (gmail if not set)
#'
#' @examples
#' if (exists("not_run")) {
#'   tictoc::tic()
#'   Sys.sleep(1)
#'   jrrosell::notify_finished("job", "Well done", sound = "fanfare", tictoc_result = tictoc::toc())
#' }
#'
#' @export
notify_finished <- \(name, body = "", ..., sound = 1, tictoc_result = NULL) {
  if (!requireNamespace("beepr", quietly = TRUE)) {
    return(NULL)
  }
  if (!requireNamespace("blastula", quietly = TRUE)) {
    return(NULL)
  }
  if (!requireNamespace("glue", quietly = TRUE)) {
    return(NULL)
  }
  beepr::beep(sound)
  elapsed <- ""
  if (length(tictoc_result) > 0) {
    elapsed <- round(tictoc_result$toc - tictoc_result$tic)
    body <- blastula::md(glue::glue("{elapsed}s elapsed.
{body}"))
  }
  provider <- ifelse(!is.null(Sys.getenv("MY_SMTP_PROVIDER")), Sys.getenv("MY_SMTP_PROVIDER"), "gmail")
  blastula::compose_email(blastula::md(glue::glue("{body}

Sent from R at {Sys.time()}")), ...) |>
    blastula::smtp_send(
      subject = glue::glue("Finished {name}"),
      from = Sys.getenv("MY_SMTP_USER"),
      to = Sys.getenv("MY_SMTP_RECIPIENT"),
      credentials = blastula::creds_envvar(
        user = Sys.getenv("MY_SMTP_USER"),
        pass_envvar = "MY_SMTP_PASSWORD",
        provider = provider
      )
    )
}



#' Name unnamed chunks in .Rmd or .qmd files
#' 
#' Use with caution. It will overwrite your files.
#' 
#' @rdname name_unnamed_chunks
#' @keywords processing
#' @param file_path the file name
#' @export
name_unnamed_chunks <- function(file_path) {
  lines <- readLines(file_path)
  for (i in seq_along(lines)) {
    line <- lines[i]
    if (!is_named_chunk(line)) {
      lines[i] <- rename_chunk(line)
    }
  }
  writeLines(lines, file_path)
  message("Unnamed chunks renamed successfully!")
}


#' @noRd
generate_weird_name <- function() {
  adjectives <- c("bold", "brave", "calm", "clever", "eager", "fancy", "friendly", "gentle", "happy", "jolly")
  animals <- c("elephant", "fox", "giraffe", "hippo", "jaguar", "koala", "lemur", "moose", "otter", "panda")
  paste(sample(adjectives, 1), sample(animals, 1), round(stats::runif(1, 10, 999)), sep = "_")
}

#' @noRd
is_named_chunk <- function(chunk_string) {
  pattern <- "^\\s*```\\{r\\s+([a-zA-Z_][a-zA-Z0-9_]*)(,.*)?\\}"
  return(grepl(pattern, chunk_string))
}

#' @noRd
rename_chunk <- function(line) {
  unique_name <- generate_weird_name()
  if (grepl("```\\{r\\s*,", line)) {
    new_line <- sub("^(```\\{r)(\\s*)(.*?)(\\})", paste0("\\1 ", unique_name, "\\2\\3"), line)
  } else {
    new_line <- sub("^(```\\{r)(\\s*)", paste0("\\1 ", unique_name, ""), line)
  }
  return(new_line)
}
