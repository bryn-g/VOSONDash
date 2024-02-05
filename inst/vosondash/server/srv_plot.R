source("server/srv_plot_layout.R", local = TRUE)
source("server/srv_plot_igraph.R", local = TRUE)
source("server/srv_plot_visnet.R", local = TRUE)

# observeEvent({
#   input$overlay_summary_chk
#   input$overlay_legend_chk
#   input$overlay_dl_btns_chk
# }, {
#   shinyjs::toggle(id = "graph_summary_ui", condition = input$overlay_summary_chk)
#   shinyjs::toggle(id = "graph_legend_ui", condition = input$overlay_legend_chk)
#   shinyjs::toggle(id = "visnet_html_dl_btn", condition = input$overlay_dl_btns_chk)
#   shinyjs::toggle(id = "graph_graphml_dl_btn", condition = input$overlay_dl_btns_chk)
# }, ignoreInit = TRUE)
# 
# observeEvent(input$plot_height, {
#   graph_rv$plot_height <- input$plot_height
# }, ignoreInit = TRUE)

output$vis_plot_ui <- renderUI({
  tabBox(width = 12, title = span(icon("share-nodes", class = "social_green"), "Network Graphs"), 
         selected = input$selected_graph_tab, id = "selected_graph_tab",
         tabPanel("igraph",
                  plotOutput("plot_igraph", width = "100%", height = "auto"),
                  value = "Plot"),
         tabPanel("visNetwork", visNetworkOutput("visNetworkPlot", width = "100%",
                                                 height = paste0(graph_rv$plot_height, "px")), value = "visNetwork"),
         tabPanel(icon("circle-info"),
                  value = "graph_info",
                  htmlOutput("graph_info_text", style = paste0("height:", graph_rv$plot_height, "px")))
  )
})

output$graph_legend_ui <- renderUI({
  tagList(div(div(
    HTML(r_graph_legend()),
    style = paste0("position:absolute; z-index:1; top:", as.numeric(graph_rv$legend_height),
                   "px; left:18px; font-size:0.97em;")),
    style = "position:relative; z-index:0;"))
})

output$graph_summary_ui <- renderUI({
  tagList(div(div(
    HTML(r_graph_summary_html()),
    style = paste0("position:absolute; z-index:1; top:", (as.numeric(graph_rv$plot_height)-5), 
                   "px; left:18px; font-size:0.97em;")),
    style = "position:relative; z-index:0;"))
})

# plots cannot be objects?
output$plot_igraph <- renderPlot({
  r_graph_igraph_plot()
}, height = function() as.numeric(graph_rv$plot_height), res = 96) # res = 109 consider allowing to change to save plots

output$visNetworkPlot <- renderVisNetwork({
  r_graph_visnet_plot()
})

r_graph_legend <- reactive({
  req(r_graph_filtered())
  
  g <- r_graph_filtered()
  output <- c()
  
  if (!is.null(g)) {
    isolate({
      categorical_attributes <- graph_rv$graph_cats
      selected_categorical_attribute <- input$graph_cat_select
    })
    
    output <- append(output, paste0(""))
    
    if (length(categorical_attributes) > 0) {
      if (nchar(selected_categorical_attribute) && selected_categorical_attribute != "All") {
        categories <- categorical_attributes[[selected_categorical_attribute]]
        df <- data.frame('cat' = categories)
        if (nrow(df) > 0) {
          if (!("color" %in% igraph::vertex_attr_names(g) & input$use_node_colors_check == TRUE)) {
            output <- append(output, paste0("<table><tbody><tr><td colspan='3'>",
                                            selected_categorical_attribute, "</td></tr>"))
            df$color <- gbl_plot_palette()[1:nrow(df)]
            for (row in 1:nrow(df)) {
              output <- append(output,
                               paste0("<tr><td style='vertical-align:middle'>",
                                      "<span style='height:12px; width:12px; border-radius:50%; display:inline-block;",
                                      "background-color:", df[row, 2], ";'></span></td>",
                                      "<td>&nbsp;</td><td style='vertical-align:middle'>", df[row, 1], "</td></tr>"))
            }
            output <- append(output, "</tbody></table>")
          }
        }
      }
    }
    
  } else {
    output <- append(output, paste0(""))
  }
  
  output
})
