#' @title Get the vosonSML package version
#' 
#' @description This function returns the version of the loaded vosonSML package.
#' 
#' @return Package version as character string.
#' 
#' @keywords internal
#' @export
get_voson_ver <- function() {
  if ("vosonSML" %in% loadedNamespaces()) return(utils::packageVersion("vosonSML"))
  "unknown"
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
  stopifnot(
    "api_key must be a character string." = (is.character(api_key) && length(api_key) == 1),
    "video_ids must be a vector of character strings." = is.character(video_ids),
    "max_comments must be numeric, > 0 and not infinite." = (
      is.numeric(max_comments) && max_comments > 0 && !is.infinite(min)
    )
  )
  
  params <- list()

  cred <- vosonSML::Authenticate("youtube", apiKey = api_key)
  
  params[["credential"]] <- cred
  params[["videoIDs"]] <- video_ids
  
  if (is.numeric(max_comments) && max_comments > 1) params["maxComments"] <- max_comments
  
  params["writeToFile"] <- FALSE
  params["verbose"] <- TRUE
  
  do.call(vosonSML::Collect, params)
}

get_errors <- function(errs) {
  sapply(errs, function(x) ifelse(x[1], NULL, x[2]))
}

#' @title Collect reddit data
#' 
#' @description This function is a wrapper for collecting reddit thread comments using \code{vosonSML::Collect}. 
#' 
#' @param thread_urls Character vector. Thread urls to collect comments from.
#' @param sort_type Character vector. Sort type for comments in each thread.
#'
#' @return A vosonSML reddit dataframe.
#' 
#' @keywords internal
#' @export
get_reddit_data <- function(thread_urls = NULL, sort_type = NULL) {
  errs <- get_errors(
    c((is.character(api_key) && length(api_key) == 1), "api_key must be a character string."),
    c(is.character(video_ids), "video_ids must be a vector of character strings."),
    c((is.numeric(max_comments) && max_comments > 0 && !is.infinite(min)),
      "max_comments must be numeric, > 0 and not infinite.")
    )
  if (length(errs)) {
    message(paste0(errs, collapse = "\n"))
    return(NULL)
  }
  
  sort_type <- replace(sort_type, sort_type == "na", NA)
  
  if (length(thread_urls) > 0) {
    data <- vosonSML::Collect(vosonSML::Authenticate("reddit"),
                              threadUrls = thread_urls,
                              sort = sort_type,
                              waitTime = 5, 
                              writeToFile = FALSE,
                              verbose = TRUE)
  }
  
  data
}

#' @title Create reddit actor networks
#' 
#' @description This function is a wrapper for creating a reddit actor networks using \code{vosonSML::Create}.
#' 
#' @param data \pkg{vosonSML} reddit dataframe.
#'
#' @return Reddit actor networks as named list.
#' 
#' @keywords internal
#' @export
createRedditActorNetwork <- function(data) {
  network <- vosonSML::Create(data, "actor", writeToFile = FALSE)
  networkWT <- vosonSML::Create(data, "actor", textData = TRUE, cleanText = TRUE, verbose = TRUE, writeToFile = FALSE)
  
  list(network = network$graph, networkWT = networkWT$graph)
}

#' @title Collect mastodon data
#' 
#' @description This function is a wrapper for collecting mastodon thread comments using \code{vosonSML::Collect}. 
#' 
#' @param mastodon_url_list Character vector. Thread urls to collect comments from.
#'
#' @return A vosonSML mastodon dataframe.
#' 
#' @keywords internal
#' @export
collectMastodonThreadData <- function(mastodon_url_list) {
  data <- NULL

  if (length(mastodon_url_list) > 0) {
    data <- 
      vosonSML::Authenticate("mastodon", verbose = FALSE) |>
      vosonSML::Collect(
        endpoint = "thread",
        threadUrls = mastodon_url_list,
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
collectMastodonSearchData <- function(hashtag, instance, local = FALSE, n = 100) {
  data <- NULL
  
  if (hashtag == "") hashtag <- NULL
  
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
