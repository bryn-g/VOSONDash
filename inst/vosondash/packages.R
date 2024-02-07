# required packages
required_pkgs <- c(
  "colourpicker",
  "dplyr",
  "DT",
  "htmlwidgets",
  "igraph",
  "purrr",
  "RColorBrewer",
  "shinydashboard",
  "shinyjs",
  "shinyWidgets",
  "sortable",
  "tm",
  "visNetwork",
  "wordcloud"
)

# if app is local print package information
if (isLocal) {
  message(
    paste0("VOSONDash", " v", utils::packageVersion("VOSONDash"), "\n",
           packageDescription("VOSONDash", fields = c("Built")), "\n\n"),
    paste0(trimws(paste(Sys.getenv("os"), R.Version()$platform)), "\n"),
    paste0(R.version.string, "\n"),
    paste0("shiny v", utils::packageVersion("shiny"), "\n"),
    paste0("vosonSML v", utils::packageVersion("vosonSML"), "\n"),
    paste0("\nHome: ", Sys.getenv("HOME"), "\n\n"),
    paste0("Checking packages...\n")
  )
}

req_pkgs <- function(required_pkgs, msgs = TRUE) {
  if (msgs) return(sapply(required_pkgs, function(x) require(x, character.only = TRUE)))
  sapply(required_pkgs, function(x) suppressWarnings(suppressPackageStartupMessages(require(x, character.only = TRUE))))
}
# load required shiny app packages
loaded_pkgs <- req_pkgs(required_pkgs, msgs = pkgMsgs)

stop_msg <- function(missing_pkgs) {
  err_msg <- paste0(
    "Required packages missing.\n",
    paste0(sapply(missing_pkgs, function(x) paste0("- ", x)), collapse = "\n"),
    "\n\nPlease install required packages before using VOSONDash:\n\n"
  )
  
  pkgs <- sapply(missing_pkgs, function(x) paste0("\"", x, "\""))
  err_msg <- paste0(err_msg, paste0("install.packages(c(", paste0(pkgs, collapse = ","), "))\n"), "\n")
  
  stop(err_msg, call. = FALSE)
}

# shiny server running locally
if (isLocal) {
  if (any(loaded_pkgs == FALSE)) {
    missing_pkgs <- names(which(loaded_pkgs == FALSE))
    
    message(paste0("Required packages are missing, please install before continuing.\n",
                   paste0(missing_pkgs, collapse = ", "), "\n"))
    
    if (interactive()) {
      rlang::check_installed(missing_pkgs)
      
      if (!rlang::is_installed(missing_pkgs)) stop("Packages still missing.\n", call. = FALSE)
    } else {
      stop_msg(missing_pkgs)
    }
  } else {
    message("Found all required packages.\n")
  }
  
  if (rlang::is_installed("praise")) message(
    do.call(get("praise", envir = loadNamespace("praise")),
      list("${EXCLAMATION}! You have done this ${adverb_manner}!")))

  message("Starting VOSONDash...\n")
}
