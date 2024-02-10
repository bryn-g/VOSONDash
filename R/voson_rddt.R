#' @title Get a reddit thread id from url
#' 
#' @description This function extracts the thread id from a reddit thread url.
#' 
#' @param url Character string. Reddit thread url.
#' 
#' @return Reddit thread id as character string.
#'
#' @export
get_rddt_url_id <- function(url) {
  thread_id <- gsub("^(.*)?/comments/([0-9A-Za-z]{6})?/.*?(/)?$", "\\2",url, ignore.case = TRUE, perl = TRUE)
}

#' @title Get subreddit name from url
#' 
#' @description This function extracts the subreddit name from a reddit thread url. 
#' 
#' @param url Character string. Reddit thread url.
#' 
#' @return Subreddit name as character string.
#'
#' @export
get_rddt_url_subrddt <- function(url) {
  subreddit <- gsub("^(.*)?/r/(.*)?/comments/.*?(/)?$", "\\2", url, ignore.case = TRUE, perl = TRUE)  
}

#' @title Get reddit thread listings for one or more subreddits
#' 
#' @description This function is a wrapper for collecting reddit subreddit listings using \code{vosonSML::Collect}. 
#' 
#' @param subreddits Character vector. Subreddit urls to collect threads from.
#' @inheritDotParams vosonSML::collect_reddit_listings sort period max waitTime ua
#'
#' @return A vosonSML subreddit listing dataframe.
#' 
#' @export
get_rddt_lst_data <- function(subreddits = NULL, sort = "new", period = "all", max = 25, ...) {
  if (is.null(subreddits)) return(NULL)
  if (length(subreddits) < 1) return(NULL)
  
  sort <- replace(sort, sort == "na", NA)
  
  data <- vosonSML::Collect(
    vosonSML::Authenticate("reddit"),
    endpoint = "listing", 
    subreddits = subreddits, 
    sort = sort,
    period = "all",
    max = 25,
    waitTime = c(6, 8),
    writeToFile = FALSE,
    verbose = FALSE,
    ...
  )
  
  data
}

#' @title Get reddit thread data
#' 
#' @description This function is a wrapper for collecting reddit thread comments using \code{vosonSML::Collect}. 
#' 
#' @param urls Character vector. Thread urls to collect comments from..
#' @inheritDotParams vosonSML::collect_reddit_threads sort waitTime ua

#' @return A vosonSML reddit dataframe.
#' 
#' @export
get_rddt_data <- function(urls = NULL, sort = NA, ...) {

  sort <- replace(sort, sort == "na", NA)
  
  if (length(urls) < 1) return(NULL)
  
  data <- vosonSML::Collect(
    vosonSML::Authenticate("reddit"),
    threadUrls = urls,
    sort = sort,
    waitTime = 5,
    writeToFile = FALSE,
    verbose = TRUE,
    ...
  )
}

