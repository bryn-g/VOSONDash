#' @title Run the VOSON Dashboard Shiny Application
#' 
#' @description This function launches the \pkg{VOSONDash} Shiny app in the default web browser. 
#' 
#' @param startup_msgs Logical. Display app package loading messages. Default is \code{FALSE}.
#' @param local Logical. Manually set app local or server mode flag.
#' @inheritDotParams shiny::runApp display.mode launch.browser

#' @return None
#' 
#' @export
dash <- function(startup_msgs = FALSE, local = NULL, ...) {
  app_dir <- system.file("", "vosondash", package = "VOSONDash")
  if (app_dir == "") {
    stop("Could not find the app try re-installing the VOSONDash package.", call. = FALSE)
  }
  
  if (!is.logical(startup_msgs)) startup_msgs = FALSE
  if (!is.logical(local)) local <- NULL
  
  # set verbose package loading by app
  # .GlobalEnv$.VOSONPkgMsgs <- startup_msgs
  # gbl_vars <- c(".VOSONPkgMsgs")
  shiny::shinyOptions(VOSONPkgMsgs = startup_msgs, VOSONIsLocal = local)
  
  # set app mode if specified
  # if (!missing(isLocal) && is.logical(isLocal)) {
    # .GlobalEnv$.VOSONIsLocal <- isLocal
    # gbl_vars <- append(gbl_vars, ".VOSONIsLocal")
    # shiny::shinyOptions(VOSONIsLocal = isLocal)
  # }
  
  # on.exit(rm(list = gbl_vars, envir = .GlobalEnv))

  # set vosonSML output to cat for console
  saved_voson_msg <- getOption("voson.msg")
  options(voson.msg = FALSE)
  
  # on.exit(rm(list = gbl_vars, envir = .GlobalEnv))
  on.exit({
    shiny::shinyOptions(VOSONPkgMsgs = NULL, VOSONIsLocal = NULL)
    options(voson.msg = saved_voson_msg)
  })
  
  shiny::runApp(app_dir, display.mode = "normal", launch.browser = TRUE, ...)
}
