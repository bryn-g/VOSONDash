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
get_word_freq_chart <- function(word_freqs,
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
get_wordcloud_plot <- function(word_freqs,
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
