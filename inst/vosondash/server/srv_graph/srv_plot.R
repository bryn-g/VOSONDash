# plot reactive variables
g_plot_rv <- reactiveValues(
  height = gbl_plot_height,
  height_legend = 42,
  width = gbl_plot_width
)

# normalize continuous values
f_norm_vals <- function(x) {
  # all values the same
  if (var(x) == 0) return(rep(0.1, length(x)))
  
  min_x <- min(x)
  diff_x <- max(x) - min_x
  s <- sapply(x, function(y) ((y - min_x) / diff_x))
}

# set hide state for overlays
observeEvent(c(
    input$canvas_tab,
    input$overlay_summary_chk,
    input$overlay_dl_btns_chk,
    input$overlay_legend_chk
  ), {
  tab <- input$canvas_tab
  
  if (!isTruthy(tab)) tab <- "igraph"
  
  if (tab %in% c("igraph", "visNetwork")) {
    if (isTruthy(r_graph_filter())) {
      shinyjs::toggle("graph_summary_ui", condition = input$overlay_summary_chk)
      
      shinyjs::enable("graph_graphml_dl_btn")
      shinyjs::toggle("graph_graphml_dl_btn", condition = input$overlay_dl_btns_chk)
      
      if (tab == "visNetwork") {
        g_plot_rv$height_legend <- ifelse(input$visnet_id_sel_chk, 85, 42)
        enable_visnet_ctrls()
        
        shinyjs::toggle("visnet_html_dl_btn", condition = input$overlay_dl_btns_chk)
      } else {
        g_plot_rv$height_legend <- 42
        enable_igraph_ctrls()
      }
      
      shinyjs::toggle("graph_legend_ui", condition = input$overlay_legend_chk)
    } else {
      shinyjs::disable("graph_graphml_dl_btn")
      shinyjs::hide("graph_graphml_dl_btn")
      
      shinyjs::disable("visnet_html_dl_btn")
      shinyjs::hide("visnet_html_dl_btn")
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

# init
observeEvent(input$canvas_tab, {
  addCssClass(selector = "a[data-value = 'igraph']", class = "inactive_menu_link")
  addCssClass(selector = "a[data-value = 'visNetwork']", class = "inactive_menu_link")
  addCssClass(selector = "a[data-value = 'graph_info']", class = "inactive_menu_link")
}, once = TRUE)

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
    selected = isolate(input$canvas_tab), # "voson_info", # input$canvas_tab,
    id = "canvas_tab",
    tabPanel(
      "igraph",
      id = "igraph",
      plotOutput("plot_igraph", width = "100%", height = "auto"),
      value = "igraph"
    ),
    tabPanel(
      "visNetwork",
      id = "visNetwork",
      visNetworkOutput(
        "plot_visnet",
        width = "100%",
        height = paste0(g_plot_rv$height, "px")
      ),
      value = "visNetwork"
    ),
    tabPanel(
      icon("circle-info"),
      value = "graph_info",
      htmlOutput("graph_info_text", style = paste0("height:", g_plot_rv$height, "px"))
    ),
    tabPanel(
      "voson",
      value = "voson_info",
      uiOutput("voson_info_html", style = paste0("height:", g_plot_rv$height, "px"))
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

output$plot_visnet <- renderVisNetwork({
  r_graph_visnet_plot()
})

output$voson_info_html <- renderUI({
  div(
    div(
      img(src = "vosondash.png", style = "max-width:80%;max-height:80%"), br(),
      span(h3(paste("VOSON Dashboard", app_ver))),
      style = paste0("text-align:center;display:block;margin-left:auto;margin-right:auto;",
                     "position:absolute;top:25%;transform:translateX(-50%);left:50%")
    ), style = "width:100%;height:100%;margin-left:auto;margin-right:auto;background-color:#ecf0f5"
  )
})
