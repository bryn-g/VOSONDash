#' @title Create a word frequency chart
#'
#' @description This function creates a horizontal barchart of word frequencies.
#'
#' @param word_freqs Dataframe. Word frequencies.
#' @param min_freq Numeric. Minimum frequency for a word to be included in the chart. Default is \code{1}.
#' @param top_count Numeric. Top count of words to render in word frequency chart. Default is \code{20}.
#' @param pcolors List. Colors to assign categorical variable in the plot. Default is \code{NULL}.
#' @param family Character string. Set a font family for plot labels. Default is \code{NULL}.
#'
#' @return A barchart plot.
#'
#' @export
wordFreqChart <- function(word_freqs,
                          min_freq = 1,
                          top_count = 20,
                          pcolors = NULL,
                          family = NULL) {

  # returns empty plot with message if no data to chart
  if (is.null(word_freqs) || nrow(word_freqs) < 1) {
    return(get_empty_plot("No text data."))
  }

  saved_par <- par(no.readonly = TRUE)
  on.exit(par(saved_par))

  freq <- NULL
  word_freqs <- word_freqs[freq >= min_freq]
  word_freqs <- word_freqs[order(word_freqs, -freq)]
  word_freqs <- word_freqs[1:top_count, ]
  word_freqs$word <- factor(word_freqs$word, levels = rev(word_freqs$word))
  
  plot_parameters <- list(word ~ freq,
                          data = word_freqs,
                          col = pcolors,
                          xlab = "Frequency")
  
  if (!is.null(family)) { plot_parameters[["scales"]] <- list("fontfamily" = family) }
  
  par(mar = rep(0, 4))
  do.call(lattice::barchart, plot_parameters)
}

#' @title Create a wordcloud plot
#'
#' @description This function creates a wordcloud plot from word frequencies.
#'
#' @param word_freqs Table. Table of word frequencies.
#' @param seed Numeric. Seed value can be supplied to reproduce a word cloud layout.
#' @param min_freq Numeric. Minimum word frequency to include a word in the word cloud. Default is \code{1}.
#' @param max_words Numeric. Maximum number of words to render in the word cloud. Default is \code{50}.
#' @param pcolors List. Colors to assign categorical variable in the plot or palette to use if \code{random.color}.
#' Default is \code{NULL}.
#' @param family Character. Set a font family for plot labels. Default is \code{NULL}.
#' @inheritDotParams wordcloud::wordcloud random.order random.color rot.per
#'
#' @return A wordcloud plot.
#'
#' @export
wordCloudPlot <- function(word_freqs,
                          seed = NULL,
                          min_freq = 1,
                          max_words = 50,
                          pcolors = NULL,
                          family = NULL,
                          ...) {
  
  # returns empty plot with message if no data to plot
  if (is.null(word_freqs) || nrow(word_freqs) < 1) {
    return(get_empty_plot("No text data."))
  }
  
  if (!is.null(seed)) set.seed(seed)
  
  saved_par <- par(no.readonly = TRUE)
  on.exit(par(saved_par))
  
  freq <- NULL
  word_freqs <- word_freqs[order(word_freqs, freq)]
  
  plot_parameters <- list(words = word_freqs$word,
                          freq = word_freqs$freq,
                          min.freq = min_freq,
                          max.words = max_words)
  
  plot_parameters[["colors"]] <- pcolors
  
  if (!is.null(family)) plot_parameters["family"] <- family
  
  dots <- list(...)
  plot_parameters <- append(plot_parameters, dots)
  
  par(mar = rep(0, 4))
  do.call(wordcloud::wordcloud, plot_parameters)
}

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
wordFreqFromCorpus <- function(corp,
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
corpusFromGraph <- function(g = NULL,
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
  
  if (tolower(type) == "node") {
    txt_data <- igraph::vertex_attr(g, txt_attr)
  } else {
    txt_data <- igraph::edge_attr(g, txt_attr)
  }

  if (iconv) {
    utf_str <- ifelse(isMac(), "utf-8-mac", "utf-8")
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
  
  corp <- tm::tm_map(corp, tm::stripWhitespace, lazy = TRUE)
}
