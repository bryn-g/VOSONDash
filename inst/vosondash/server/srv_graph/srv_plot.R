source("server/srv_graph/srv_plot_layout.R", local = TRUE)
source("server/srv_graph/srv_plot_igraph.R", local = TRUE)
source("server/srv_graph/srv_plot_visnet.R", local = TRUE)

# set hide state for overlays
observeEvent(c(
    g_rv$is_igraph,
    input$canvas_tab,
    input$overlay_summary_chk,
    input$overlay_dl_btns_chk,
    input$overlay_legend_chk
  ), {
  tab <- input$canvas_tab
  
  if (!isTruthy(tab)) tab <- "igraph"
  
  if (tab %in% c("igraph", "visNetwork")) {
    if (isTruthy(g_rv$is_igraph)) {
      shinyjs::toggle("graph_summary_ui", condition = input$overlay_summary_chk)
      
      shinyjs::enable("graph_graphml_dl_btn")
      shinyjs::toggle("graph_graphml_dl_btn", condition = input$overlay_dl_btns_chk)
      
      if (tab == "visNetwork") {
        g_plot_rv$height_legend <- ifelse(input$visnet_id_sel_chk, 85, 42)
        disable_igraph_ctrls()
        
        shinyjs::enable("visnet_html_dl_btn")
        shinyjs::toggle("visnet_html_dl_btn", condition = input$overlay_dl_btns_chk)
      } else {
        g_plot_rv$height_legend <- 42
        enable_igraph_ctrls()
      }
      
      shinyjs::toggle("graph_legend_ui", condition = input$overlay_legend_chk)
    }
  } else {
    shinyjs::hide("graph_summary_ui")
    shinyjs::hide("graph_legend_ui")
    
    shinyjs::disable("graph_graphml_dl_btn")
    shinyjs::hide("graph_graphml_dl_btn")
    
    shinyjs::disable("visnet_html_dl_btn")
    shinyjs::hide("visnet_html_dl_btn")
  }
})

# graph summary text
r_graph_summary_html <- reactive({
  g <- r_graph_filtered()
  if (is.null(g)) return(NULL)
  paste0(c(
    paste("Nodes:", igraph::vcount(g)),
    paste("Edges:", igraph::ecount(g)),
    paste("Isolates:", sum(igraph::degree(g) == 0))
  ), collapse = "<br>")
})

# update plot height
observeEvent(input$plot_height, {
  g_plot_rv$height <- input$plot_height
}, ignoreInit = TRUE)

# when updated plot width
observeEvent(g_plot_rv$width, {
  req(g_plot_rv$width)
  updateTabsetPanel(session, "selected_graph_controls_tab", selected = "Visual")
  updateTabsetPanel(session, "selected_visuals_tab", selected = "Canvas")
}, ignoreInit = TRUE, ignoreNULL = TRUE)

# enable canvas size btn
observeEvent(input$plot_col_size, {
  if (input$plot_col_size != g_plot_rv$width) {
    shinyjs::enable("plot_col_size_btn")
  } else {
    shinyjs::disable("plot_col_size_btn")
  }
}, ignoreInit = TRUE, ignoreNULL = TRUE)

# update plot width
observeEvent(input$plot_col_size_btn, {
  req(input$plot_col_size_btn, input$plot_col_size)
  
  prev_width <- g_plot_rv$width
  new_width <- as.numeric(input$plot_col_size)
  
  if (new_width == prev_width | new_width > 11 | new_width < 1) return(NULL)
  
  g_plot_rv$width <- new_width
  
  # perhaps can occur dynamically as with screen size change
  # ctrl_cls_add <- paste0("sm-col-", ctrl_width)
  # plot_cls_add <- paste0("sm-col-", plot_width)
  # 
  # cat(file=stderr(), paste0("debug: plot_cls_add - ", plot_cls_add, "\n"))
  # cat(file=stderr(), paste0("debug: ctrl_cls_add - ", ctrl_cls_add, "\n"))
  # 
  # shinyjs::addClass(id = "plot_column", class = plot_cls_add)
  # shinyjs::addClass(id = "ctrl_column", class = ctrl_cls_add)
  # 
  # x <- paste0("sm-col-", (12 - prev_width))
  # y <- paste0("sm-col-", prev_width)
  # shinyjs::removeClass(id = "ctrl_column", class = x)
  # shinyjs::removeClass(id = "plot_column", class = y)
  # 
  # shinyjs::addClass(id = "ctrl_column", class = "xyzabcd")
  
}, ignoreInit = TRUE, ignoreNULL = TRUE)

# update legend position
observeEvent(input$visnet_id_sel_chk, {
  g_plot_rv$height_legend <- ifelse(input$visnet_id_sel_chk, 85, 42)
})

output$vis_plot_ui <- renderUI({
  tabBox(
    width = 12,
    title = span(icon("share-nodes", class = "social_green"), "Network Graphs"),
    selected = input$canvas_tab,
    id = "canvas_tab",
    tabPanel(
      "igraph",
      plotOutput("plot_igraph", width = "100%", height = "auto"),
      value = "igraph"
    ),
    tabPanel(
      "visNetwork",
      visNetworkOutput(
        "visNetworkPlot",
        width = "100%",
        height = paste0(g_plot_rv$height, "px")
      ),
      value = "visNetwork"
    ),
    tabPanel(
      icon("circle-info"),
      value = "graph_info",
      htmlOutput("graph_info_text", style = paste0("height:", g_plot_rv$height, "px"))
    )
  )
})

output$graph_legend_ui <- renderUI({
  tagList(div(div(
    HTML(r_graph_legend()),
    style = paste0(
      "position:absolute; z-index:1; top:",
      as.numeric(g_plot_rv$height_legend),
      "px; left:18px; font-size:0.97em;"
    )
  ),
  style = "position:relative; z-index:0;"))
})

output$graph_summary_ui <- renderUI({
  tagList(div(div(
    HTML(r_graph_summary_html()),
    style = paste0(
      "position:absolute; z-index:1; top:",
      (as.numeric(g_plot_rv$height) - 5),
      "px; left:18px; font-size:0.97em;"
    )
  ),
  style = "position:relative; z-index:0;"))
})

# standard plots cannot be treated as objects
# res = 109 consider allowing to change res to save plots at
output$plot_igraph <- renderPlot({
  r_graph_igraph_plot()
}, height = function() as.numeric(g_plot_rv$height), res = 96)

output$visNetworkPlot <- renderVisNetwork({
  r_graph_visnet_plot()
})
