# graph download buttons
output$visnet_html_dl_btn <- downloadHandler(
  filename = function() { saveGraphFileName() },
  content = function(file) {
    visNetwork::visSave(r_graph_visnet_save_data(), file, selfcontained = TRUE, background = "white")
  }
)

# analysis graphml download button
output$graph_graphml_dl_btn <- downloadHandler(
  filename = function() { create_dtm_filename("analysis-graph", "graphml") },
  content = function(file) { igraph::write_graph(r_graph_filtered(), file, format = c("graphml")) }
)

dlGraphButtonOutput <- reactive({
  tagList(tags$div(downloadButton("graph_graphml_dl_btn", label = "Graphml", title = "Download Plot Graphml File"), 
                   style = "float:right; margin-right:10px; margin-bottom:0px;", class = "div_inline"),
          tags$div(downloadButton("visnet_html_dl_btn", label = "Plot HTML", title = "Download Plot as HTML File"), 
                   style = "float:right; margin-right:10px; margin-bottom:0px;", class = "div_inline"))
})

output$graph_dl_button_ui <- renderUI({
  tagList(div(div(
    dlGraphButtonOutput(),
    style = paste0("position:absolute; z-index:1; top:", (as.numeric(g_plot_rv$height)+16),
                   "px; right:14px; font-size:0.9em;")),
    style = "position:relative; z-index:0;"))
})

# network graph save file name based on selected network graph tab
saveGraphFileName <- reactive({
  switch(input$canvas_tab,
         "visNetwork" = create_dtm_filename("visNetwork-graph", "html"))
})

# network graph data based on selected network graph tab
# saveGraphFileData
r_graph_visnet_save_data <- reactive({
  data <- switch(input$canvas_tab,
                 "visNetwork" = r_graph_visnet_plot())
  
  if (input$canvas_tab == "visNetwork") {
    data$height <- "800px"
    data$sizingPolicy$defaultWidth <- "100%"
    
    data$sizingPolicy$browser$fill <- TRUE
    data$sizingPolicy$viewer$suppress <- TRUE
    data$sizingPolicy$knitr$figure <- FALSE    
  }
  
  data
})
