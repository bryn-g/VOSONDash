#' @title Get a youtube video id from url
#' 
#' @description This function extracts the youtube video id from a youtube video url.
#' 
#' @param url Character string. Youtube video url.
#' 
#' @return Video id as character string.
#'
#' @export
get_ytbe_video_id <- function(url) {
  # already an id
  if (grepl("^[0-9A-Za-z_\\-]{11}$", url, ignore.case = TRUE, perl = TRUE)) return(url)
  
  url <- httr::parse_url(url)
  id <- NULL
  
  if (is.null(url$hostname)) return(NULL)
  
  # https://youtu.be/xxxxxxxxxxx
  if (tolower(trimws(url$hostname)) == "youtu.be") {
    if (length(url$path) > 0) id <- url$path[1]
  }
  
  # https://www.youtube.com/watch?v=xxxxxxxxxxx
  if (tolower(trimws(url$hostname)) == "www.youtube.com") {
    if (!is.null(url$query$v)) id <- url$query$v
  }
  
  # check valid id
  if (!grepl("^[0-9A-Za-z_\\-]{11}$", id, ignore.case = TRUE, perl = TRUE)) return(NULL)
  
  id
}

#' @title Collect youtube data
#' 
#' @description This function is a wrapper for collecting youtube video comments via \code{vosonSML::Collect}. 
#' 
#' @param api_key Character string. Youtube api key.
#' @param video_ids Character vector. Youtube video ids to collect comments from.
#' @param max_comments Numeric. Maximum number of comments to collect.
#' 
#' @return A vosonSML youtube dataframe.
#' 
#' @keywords internal
#' @export
get_youtube_data <- function(api_key = NULL, video_ids = NULL, max_comments = NULL) {
  cred <- vosonSML::Authenticate("youtube", apiKey = api_key)
  
  params <- list(
    "credential" = cred,
    "videoIDs" = video_ids,
    "maxComments" = ifelse((is.numeric(max_comments) && max_comments > 1), max_comments, NULL),
    "writeToFile" = FALSE,
    "verbose" = FALSE
  )
  
  do.call(vosonSML::Collect, params)
}

