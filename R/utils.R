#' @title Get loaded voson package versions
#' 
#' @description This function returns the version of the vosonSML and VOSONDash packages.
#' 
#' @return Package versions as named list.
#' 
#' @keywords internal
#' @export
get_voson_vers <- function() {
  sapply(list(VOSONDash = "VOSONDash", vosonSML = "vosonSML"),
    function(x) ifelse(x %in% loadedNamespaces(), paste0(utils::packageVersion(x), collapse = "."), "unknown"),
    USE.NAMES = TRUE, simplify = FALSE)
}

#' @title Create a file name with system datetime prefix
#' 
#' @description This function uses the system date and time to create a unique file name.
#' 
#' @param suffix Character string. Name part of file name to append to datetime part.
#' @param ext Character string. File extension without the period. For example, \code{"txt"}.
#' @param clean Logical. Remove problematic file system characters from file name part. Default is \code{FALSE}.
#' 
#' @return A unique datetime filename as character string.
#' 
#' @keywords internal
#' @export
create_dtm_filename <- function(suffix, ext, clean = FALSE) {
  systime <- Sys.time()
  
  if (clean) {
    pattern <- c("\\s+", ":", "\\.")
    suffix <- gsub(paste0(pattern[1-2], collapse = "|"), "_", suffix, perl = TRUE)
    ext <- gsub(paste0(pattern[1-3], collapse = "|"), "", ext, perl = TRUE)
  }
  
  filename <- paste0(format(systime, "%Y-%m-%d_%H-%M-%S"), "_", suffix, ".", ext, sep = "")
}

#' @title Check if macOS
#' 
#' @description This function checks if running the macOS version of R.
#' 
#' @return Result as logical.
#' 
#' @keywords internal
#' @export
is_macos <- function() {
  is_darwin <- grep("darwin", R.Version()$os)
  ifelse(length(is_darwin), TRUE, FALSE)
}

#' @title Check system fonts
#' 
#' @description Looks up installed system fonts.
#' 
#' @return Returns a character vector of installed system fonts.
#' 
#' @keywords internal
#' @export
get_sysfont_names <- function() {
  unique(systemfonts::system_fonts()$family)
}

#' @title Add message to log queue
#'
#' @description This function adds a text message to a queue or list with a count limiting how many messages are stored.
#'   The queue stores max messages based on fifo.
#'
#' @param queue Character vector. Each item is a text log message.
#' @param add_msg Character string. Text log message to add to messages
#' @param txt Logical. Return messages as character string delimited with newline characters.
#' @param max Numeric. Return a message log queue of max messages, the rest are lost unless the queue was previously
#'   queue was saved..
#'
#' @return Messages as vector or character string.
#'
#' @keywords internal
#' @export
log_queue <- function(queue, add_msg, txt = FALSE, max = 20) {
  
  add_msg <- c(paste(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), add_msg))
  queue <- c(add_msg, queue)

  if (length(queue) > max) queue <- queue[1:max]
  if (txt) return(paste0(queue, collapse = "\n"))
  
  queue
}

#' @title Create an empty plot with text
#'
#' @description This function creates an empty plot that can be used to display a text message. Intended to be used in
#' a series of plots as a fill in that can maintain a plot aesthetic.
#'
#' @param msg Character string. Text message to centre on empty plot. Default text is \code{"No plot available."}.
#' @param cex Numeric. Text scale. Default is \code{1}.
#' @inheritParams graphics::plot.default
#' @inheritDotParams graphics::plot.default
#' 
#' @return An empty plot with text message.
#'
#' @keywords internal
#' @export
get_empty_plot <- function(msg = "No plot available.", cex = 1, type = "n", axes = FALSE, xlab = "", ylab = "", ...) {
  plot(1:10, 1:10, type = "n", axes = axes, xlab = "", ylab = "", ...)
  text(5, 5, msg, cex = cex)
}
