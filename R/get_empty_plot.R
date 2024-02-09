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