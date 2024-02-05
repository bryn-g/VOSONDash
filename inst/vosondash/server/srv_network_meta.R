output$graph_info_text <- renderText({
  gname <- ifelse(nchar(graph_rv$meta$name), graph_rv$meta$name, "not set")
  output <- paste0("<p>Name: ", gname, "<br>")
  
  gtype <- ifelse(nchar(graph_rv$meta$type), graph_rv$meta$type, "unknown")
  output <- paste0(output, "Type: ", gtype, "<br>")
  
  gdesc <- ifelse(nchar(graph_rv$meta$desc), HTML(graph_rv$meta$desc), "No description.")
  output <- paste0(output, "Description:<br></p><p>", gdesc, "</p>")
  
  output
})

output$graph_desc <- renderText({
  if (nchar(graph_rv$meta$desc)) {
    return(HTML(graph_rv$meta$desc))
  }
  
  HTML("No description.")
})

output$graph_name <- renderText({
  output <- ifelse(nchar(graph_rv$meta$name), graph_rv$meta$name, "not set")
  output <- paste("Name: ", output)
  
  if (nchar(graph_rv$meta$type)) {
    output <- paste0(output, "\nType: ", graph_rv$meta$type)
  } else {
    output <- paste0(output, "\nType: unknown")
  }
  
  output
})