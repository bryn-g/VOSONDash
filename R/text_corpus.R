#' @title Create a word frequency dataframe
#'
#' @description Create a word frequency dataframe from a text corpus.
#' 
#' @param corp a \pkg{tm} text corpus object.
#' @param rm_sparse Logical. Remove proportion of sparse terms. Default is \code{0.99}.
#' @param word_len Numeric vector. Min and max length of words to include. Default is \code{c(3, 26)}.
#' @param word_freq Numeric vector. Min and max frequency of words to include. Default is \code{c(1, Inf)}.
#' 
#' @return A data.table of word frequencies.
#' 
#' @export
get_word_freqs <- function(corp,
                           rm_sparse = 0.99,
                           word_len = c(3, 26),
                           word_freq = c(1, Inf)) {
  
  dtm <- tm::DocumentTermMatrix(corp, control = list(wordLengths = word_len,
                                                     bounds = list(global = word_freq)))
  dtm_sparse_removed <- tm::removeSparseTerms(dtm, rm_sparse)
  
  freq_terms <- colSums(as.matrix(dtm_sparse_removed))
  freq_terms <- freq_terms[order(freq_terms, decreasing = TRUE)]
  
  data.table(word = names(freq_terms), freq = freq_terms)
}

#' @title Create a text corpus from graph text attribute data
#'
#' @description This function creates a text corpus from node or edge text attribute data in an igraph. 
#' 
#' @param g an \pkg{igraph} graph object.
#' @param txt_attr Character string. Name of graph text attribute. Default is \code{NULL}.
#' @param type Character string. Graph attribute type. Default is \code{"node"}.
#' @param iconv Logical. Use the \code{iconv} function to attempt UTF8 conversion. Default is \code{FALSE}.
#' @param html_decode Logical. HTML decode text. Default is \code{TRUE}.
#' @param rm_url Logical. Remove URL's. Default is \code{TRUE}.
#' @param rm_num Logical. Remove numbers. Default is \code{TRUE}.
#' @param rm_punct Logical. Remove punctuation. Default is \code{TRUE}.
#' @param sw_kind Character string. Stopword dictionary. Refer \code{stopwords} \code{kind} parameter.
#' Default is \code{"SMART"}.
#' @param rm_words Character vector. User defined stopwords. Default is \code{NULL}.
#' @param stem Logical. Apply word stemming. Default is \code{FALSE}.
#' 
#' @return A \pkg{tm} text corpus object.
#' 
#' @export
graph_text_as_corpus <- function(g = NULL,
                        txt_attr = NULL,
                        type = "node",
                        iconv = FALSE,
                        html_decode = TRUE,
                        rm_url = TRUE,
                        rm_num = TRUE,
                        rm_punct = TRUE,
                        sw_kind = "SMART",
                        rm_words = NULL,
                        stem = FALSE) {
  
  if (is.null(g) | is.null(txt_attr) || !igraph::is_igraph(g)) return(NULL)
  
  txt_data <- ifelse((tolower(type) == "node"), igraph::vertex_attr(g, txt_attr), igraph::edge_attr(g, txt_attr))

  if (iconv) {
    utf_str <- ifelse(is_macos(), "utf-8-mac", "utf-8")
    txt_data <- iconv(txt_data, to = utf_str)
  }
  
  rm <- which(txt_data == "")
  if (length(rm) > 0) txt_data <- txt_data[-rm]

  corp <- tm::VCorpus(tm::VectorSource(txt_data))
  if (html_decode) corp <- tm::tm_map(corp, tm::content_transformer(textutils::HTMLdecode))
  
  corp <- tm::tm_map(corp, tm::content_transformer(tolower))
  if (rm_url) corp <- tm::tm_map(corp, tm::content_transformer(function(x) gsub("http[s]?://[^[:space:]]+", "", x)))
  
  # if (rm_twit_hashtags) corp <- tm::tm_map(corp, tm::content_transformer(function(x) gsub("#\\S+", "", x)))
  # if (rm_twit_users) corp <- tm::tm_map(corp, tm::content_transformer(function(x) gsub("@\\S+", "", x)))
  
  if (rm_num) corp <- tm::tm_map(corp, tm::removeNumbers)
  if (rm_punct) corp <- tm::tm_map(corp, tm::removePunctuation)
  
  sw <- c()
  if (!is.null(sw_kind) & is.character(sw_kind)) sw <- tm::stopwords(sw_kind)
  
  if (!is.null(rm_words)) {
    rm_words <- trimws(unlist(strsplit(tolower(rm_words), ",")))
    sw <- c(sw, rm_words)
  }
  
  if (length(sw) > 0) corp <- tm::tm_map(corp, tm::removeWords, sw, lazy = TRUE)
  
  if (stem) corp <- tm::tm_map(corp, tm::stemDocument)
  
  tm::tm_map(corp, tm::stripWhitespace, lazy = TRUE)
}
