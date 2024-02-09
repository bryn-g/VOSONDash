#' @title Get word frequencies
#'
#' @description Calculates word frequencies for a list or dataframe of words. Sets words as factors for top down highest
#'   to lowest word frequency.
#'
#' @param data Dataframe. Word frequencies.
#' @param min Numeric. Minimum frequency for a word to be included in the results. Default is \code{1}.
#' @param top_n Numeric. Top number of words to return. Default is \code{20}.
#' 
#' @return A dataframe of factored words.
#' 
get_word_freqs <- function(data, min = 1, top_n = NULL) {
  stopifnot(
    "`min` must be numeric" = is.numeric(min),
    "`min` must be > 0" = (top_n > 0),
    "`min` cannot be infinite" = (!is.infinite(min))
  )
  if (!is.null(top_n))
    stopifnot(
      "`top_n` must be numeric" = is.numeric(top_n),
      "`top_n` must be > 1" = (top_n > 1)
    )
  f <- NULL
  freqs <- data[f >= min]
  freqs <- freqs[order(freqs, -f)]
  if (!is.null(top_n)) freqs <- freqs[1:top_n,]
  factor(freqs$word, levels = rev(freqs$word))
}
