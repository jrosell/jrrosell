#' Download files from kaggle competition
#'
#' @rdname kgl_competitions_data_download
#' @param competition_id Number Id of the Kaggle competition.
#' @param file_name String with the file you want to download.
#' @param path String with the folder where you want the file.
#'
#' @source <https://github.com/mkearney/kaggler/issues/6>
#' @seealso <https://github.com/jrosell/jrrosell/blob/main/R/kaggle.R>
#' @export
kgl_competitions_data_download <- function(competition_id, file_name, path = ".") {
    .kaggle_base_url <- "https://www.kaggle.com/api/v1"
    url <- paste0(.kaggle_base_url, "/competitions/data/download/", competition_id, "/", file_name)
    rcall <- httr::GET(url, httr::authenticate(Sys.getenv("KAGGLE_USER"), Sys.getenv("KAGGLE_KEY"), type = "basic"))
    tmp <- tempfile()
    utils::download.file(rcall$url, tmp)
    invisible(file.copy(tmp, paste0(path, "/", file_name)))
}
