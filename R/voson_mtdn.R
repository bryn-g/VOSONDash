#' @title Collect mastodon data
#' 
#' @description This function is a wrapper for collecting mastodon thread comments using \code{vosonSML::Collect}. 
#' 
#' @param urls Character vector. Thread urls to collect comments from.
#'
#' @return A vosonSML mastodon dataframe.
#' 
#' @keywords internal
#' @export
get_mastodon_data <- function(urls) {
  data <- NULL
  
  if (length(urls) > 0) {
    data <- 
      vosonSML::Authenticate("mastodon", verbose = FALSE) |>
      vosonSML::Collect(
        endpoint = "thread",
        threadUrls = urls,
        writeToFile = FALSE,
        verbose = TRUE
      )
  }
  
  data
}

#' @title Collect mastodon data
#' 
#' @description This function is a wrapper for collecting mastodon search comments using \code{vosonSML::Collect}. 
#' 
#' @param hashtag Character string. Hashtag used to search for posts.
#' @param instance Character string. Instance to collect comments from.
#' @param local Logical. Collect posts from local or global timeline.
#' @param n Numeric. Number of posts to retrieve.
#'
#' @return A vosonSML mastodon dataframe.
#' 
#' @keywords internal
#' @export
get_mastodon_search_data <- function(hashtag, instance, local = FALSE, n = 100) {
  data <- NULL
  
  if (!isTruthy(hashtag)) return(NULL)
  
  data <- 
    vosonSML::Authenticate("mastodon", verbose = FALSE) |>
    vosonSML::Collect(
      endpoint = "search",
      hashtag = hashtag,
      instance = instance,
      local = local,
      numPosts = n,
      anonymous = TRUE,
      writeToFile = FALSE,
      verbose = TRUE
    )
  
  data
}
