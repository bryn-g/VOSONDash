#' @title Get the vosonSML package version
#' 
#' @description This function returns the version of the loaded vosonSML package.
#' 
#' @return Package version as character string.
#' 
#' @keywords internal
#' @export
getVosonSMLVersion <- function() {
  if ("vosonSML" %in% loadedNamespaces()) { return(utils::packageVersion("vosonSML")) }
  "unknown"
}

#' @title Collect youtube data
#' 
#' @description This function is a wrapper for collecting youtube video comments using \code{vosonSML::Collect}. 
#' 
#' @param youtube_api_key Character string. Youtube api key.
#' @param youtube_video_id_list Character vector. Youtube video ids to collect comments from.
#' @param youtube_max_comments Numeric. Maximum number of comments to collect.
#' 
#' @return A vosonSML youtube dataframe.
#' 
#' @keywords internal
#' @export
collectYoutubeData <- function(youtube_api_key, youtube_video_id_list, youtube_max_comments) {
  
  if (is.null(youtube_api_key) || (length(youtube_video_id_list) < 1)) {
    return(NULL)
  }
  
  collect_params <- list()

  cred <- vosonSML::Authenticate("youtube", apiKey = youtube_api_key)
  
  collect_params[["credential"]] <- cred
  collect_params[["videoIDs"]] <- youtube_video_id_list
  
  if (is.numeric(youtube_max_comments) && youtube_max_comments > 1) {
    collect_params["maxComments"] <- youtube_max_comments
  }
  
  collect_params["writeToFile"] <- FALSE
  collect_params["verbose"] <- TRUE
  
  data <- do.call(vosonSML::Collect, collect_params)
}

#' @title Create youtube actor networks
#' 
#' @description This function is a wrapper for creating a youtube actor networks using \code{vosonSML::Create}.
#' 
#' @param data \pkg{vosonSML} youtube dataframe.
#'
#' @return Youtube actor networks as named list.
#' 
#' @keywords internal
#' @export
createYoutubeNetwork <- function(data) {
  network <- Create(data, 'actor', verbose = TRUE, writeToFile = FALSE)
  
  g <- igraph::set_graph_attr(network$graph, "type", "youtube")
  
  g_wt <- g
  E(g_wt)$vosonTxt_comment <- data$Comment[match(E(g_wt)$commentId, data$CommentId)]
  
  list(network = g, networkWT = g_wt)
}

#' @title Collect reddit data
#' 
#' @description This function is a wrapper for collecting reddit thread comments using \code{vosonSML::Collect}. 
#' 
#' @param reddit_url_list Character vector. Thread urls to collect comments from.
#' @param reddit_sort Character vector. Sort type for comments in each thread.
#'
#' @return A vosonSML reddit dataframe.
#' 
#' @keywords internal
#' @export
collectRedditData <- function(reddit_url_list, reddit_sort = NA) {
  data <- NULL
  
  reddit_sort <- replace(reddit_sort, reddit_sort == "na", NA)
  
  if (length(reddit_url_list) > 0) {
    data <- vosonSML::Collect(vosonSML::Authenticate("reddit"),
                              threadUrls = reddit_url_list,
                              sort = reddit_sort,
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
