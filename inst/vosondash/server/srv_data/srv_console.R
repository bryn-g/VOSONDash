withConsoleRedirect <- function(id, value) {
  input_text <- capture.output(results <- value, type = c("output"))
  
  if (length(input_text) > 0) {
    output <- gsub("\n{2,}", "\n", input_text)
    insertUI(paste0("#", id), where = "beforeEnd",
             ui = div(id = paste0("_", id), paste0(output, "\n", collapse = ""))
    )
  }
  return(results)
}

addToConsole <- function(id, value) {
  insertUI(paste0("#", id), where = "beforeEnd",
           ui = div(id = paste0("_", id), paste0(value, "\n", collapse = ""))
  )
}

reset_console <- function(id, remove_ui = TRUE) {
  if (remove_ui) {
    removeUI(selector = paste0("div#_", id), multiple = TRUE)
  }
  vosonsml_version <- VOSONDash::get_voson_vers()$vosonSML
  if (!is.null(vosonsml_version)) {
    vosonsml_version <- paste0("vosonSML v", vosonsml_version)
  } else {
    vosonsml_version <- "vosonSML unknown"
  }
  reset_message <- paste0(vosonsml_version, "\n[Built:", gsub(" ; ", "|", packageDescription("vosonSML", fields = c("Built"))), "]\n")
  addToConsole(id, reset_message)
}
