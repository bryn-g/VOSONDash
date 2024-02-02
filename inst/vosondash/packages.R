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
    "=================================================\n",
    paste0("VOSONDash", " v", utils::packageVersion("VOSONDash"), " (", utils::packageDate("VOSONDash"), ")\n\n"),
    paste0(trimws(paste(Sys.getenv("os"), R.Version()$platform)), "\n"),
    paste0(R.version.string, "\n"),
    paste0("shiny v", utils::packageVersion("shiny"), "\n"),
    paste0("vosonSML v", utils::packageVersion("vosonSML"), "\n"),
    paste0("\nHome: ", Sys.getenv("HOME"), "\n\n"),
    paste0("Checking packages...\n")
  )
}

# load required shiny app packages
if (pkgMsgs == FALSE) {
  loaded_pkgs <- sapply(
    required_pkgs, function(x) suppressWarnings(suppressPackageStartupMessages(require(x, character.only = TRUE)))
  )
} else {
  loaded_pkgs <- sapply(required_pkgs, function(x) require(x, character.only = TRUE))
}

# shiny server running locally
if (isLocal) {
  if (any(loaded_pkgs == FALSE)) {
    missing_pkgs <- names(which(loaded_pkgs == FALSE))
    
    err_msg <- paste0(
      "Required packages missing.\n",
      paste0(sapply(missing_pkgs, function(x) paste0("- ", x)), collapse = "\n"),
      "\n\nPlease install required packages before using VOSONDash:\n\n"
    )
    
    pkgs <- sapply(missing_pkgs, function(x) paste0("\"", x, "\""))
    err_msg <- paste0(err_msg, paste0("install.packages(c(", paste0(pkgs, collapse = ","), "))\n"), "\n")
  
    stop(err_msg, call. = FALSE)
  } else {
    message("Found all required packages.\n", "\nStarting VOSONDash...\n")
  }
}
