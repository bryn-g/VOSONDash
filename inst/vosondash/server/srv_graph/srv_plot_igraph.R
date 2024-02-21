r_graph_igraph_plot <- reactive({
  g <- r_graph_visual()
  
  if (is.null(g)) return(VOSONDash::get_empty_plot("No graph data."))
  if (igraph::gorder(g) <= 0) return(VOSONDash::get_empty_plot("No nodes to plot."))
  
  # save and restore graphics parameters
  saved_par <- par(no.readonly = TRUE)  
  
  # avoid unknown font warnings on windows by setting a tt font
  saved_win_font <- NULL
  if (.Platform$OS.type == "windows") {
    saved_win_font <- windowsFonts()$Arial
    windowsFonts(Arial = windowsFont("TT Arial"))
  }
  
  # restore previously saved pars and font on exit
  on.exit({
    par(saved_par)
    if (!is.null(saved_win_font)) windowsFonts(Arial = windowsFont(saved_win_font))
  })
  
  # create a list for plot parameters
  igraph_params <- list(g)
  
  label_type <- isolate(g_nodes_rv$label_type)
  if (label_type != "None") {
    
    # set font family
    igraph_params["vertex.label.family"] <- igraph_params["edge.label.family"] <- "Arial"
    
    # set unicode font for non windows
    not_win <- .Platform$OS.type != "windows"
    arial_unicode <- "Arial Unicode MS" %in% VOSONDash::get_sysfont_names()
    
    if (not_win & arial_unicode & input$macos_font_chk) {
      igraph_params["vertex.label.family"] <- igraph_params["edge.label.family"] <- "Arial Unicode MS"
    }
  }
  
  graph_layout <- as.matrix(g_layout_rv$coords)

  x <- input$igraph_x_slider
  y <- input$igraph_y_slider
  
  graph_layout <- igraph::norm_coords(graph_layout, ymin = y[1], ymax = y[2], xmin = x[1], xmax = x[2])
  igraph_params["rescale"] <- FALSE
  
  g_spread <- input$igraph_spread_slider  
  if (isTruthy(g_spread)) graph_layout <- graph_layout * g_spread
  
  igraph_params[["layout"]] <- graph_layout

  # plot graph
  par(mar = rep(0, 4))
  do.call(plot.igraph, igraph_params)
})
