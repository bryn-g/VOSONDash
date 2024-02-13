# network metrics summary
output$net_metrics_txt <- renderText({
    r_graph_metrics()
})

# wrapper for standard distribution plot
get_std_plot <- function(x, type = "b", xlab = "", ylab = "N nodes", ...) {
  plot(x, type = type, xlab = xlab, ylab = ylab, ...)
}

# generate distribution plot
# todo: ctrl for whether to include loops in degree calculations
r_node_distrib_plot <- reactive({
  g <- r_graph_filter()
  
  if (!isTruthy(g)) return(VOSONDash::get_empty_plot("No graph data."))
  if (igraph::gorder(g) < 1) return(VOSONDash::get_empty_plot("No nodes in data."))
  
  distrib_type <- input$metrics_distrib_sel
  
  if (distrib_type == "component") {
    cc <- igraph::components(g, mode = g_comps_rv$mode)
    get_std_plot(
      table(cc$csize),
      xlab = paste0("N nodes"),
      ylab = paste0("N components [mode = ", g_comps_rv$mode, "]")
    )
  } else if (distrib_type == "component2") {
    dist <- igraph::component_distribution(g)
    get_std_plot(as.table(dist), xlab = "unknown", xlab = "unknown")
    
  } else if (distrib_type == "degree") {
    if (igraph::is_directed(g)) {
      VOSONDash::get_empty_plot("Not defined for network.")

    } else {
      get_std_plot(table(igraph::degree(g)), xlab = "Degree")

    }
  } else if (distrib_type == "indegree") {
    if (igraph::is_directed(g)) {
      get_std_plot(table(igraph::degree(g, mode = "in")), xlab = "Indegree")
    } else {
      VOSONDash::get_empty_plot("Not defined for undirected network.")
    }
  } else if (distrib_type == "outdegree") {
    if (igraph::is_directed(g)) {
      get_std_plot(table(igraph::degree(g, mode = "out")), xlab = "Outdegree")
      
    } else {
      VOSONDash::get_empty_plot("Not defined for undirected network.")
      
    }
  } else {
      VOSONDash::get_empty_plot("No graph data.")
  }
})

# degree and component distribution plot
output$metrics_distrib_plot <- renderPlot({
  r_node_distrib_plot()
})

# graph summary text
r_graph_summary_html <- reactive({
  g <- r_graph_filter()
  
  if (!isTruthy(g)) return(NULL)
  
  paste0(c(
    paste("Nodes:", igraph::gorder(g)),
    paste("Edges:", igraph::gsize(g)),
    paste("Isolates:",sum(igraph::degree(g, loops = FALSE) == 0))
  ), collapse = "<br>")
})

# format any double numbers in a list to a precision of 3 decimal places
fmt_double_values <- function(values, n = 3) {
  sapply(values, function(x, n) {
    ifelse(!is.numeric(x), x,
      ifelse(is.nan(x), x,
        ifelse(round(x) != x, format(round(x, n), nsmall = n), x)))
  }, n = n, simplify = FALSE, USE.NAMES = TRUE)
}

# format network metrics into summary text
# todo: ctrl for whether to include loops in centralization calculations
r_graph_metrics <- reactive({
  g <- r_graph_filter()

  if (!isTruthy(g)) return("No graph data.")

  metrics <- fmt_double_values(get_graph_metrics(g, mode = g_comps_rv$mode))
  out <- c(
    paste("Num of nodes:", metrics$nodes_n),
    paste("Num of edges:", metrics$edges_n),
    paste("Edges:", ifelse(metrics$directed, "directed", "undirected")),
    paste("Simple:", ifelse(metrics$simple, "yes", "no")),
    paste("Number of components:", metrics$comps_n),
    paste("Component mode:", metrics$comps_mode),
    paste("Isolates:", metrics$isos_n),
    paste("Isolates (loops):", metrics$isos_loops_n),
    paste("Density:", metrics$density),
    paste("Average geodesic distance:", metrics$ave_geodesic_dist), "",
    paste("(Global) clustering coefficient:", metrics$global_clust_coeff),
      "  Proportion of connected triples that close to\n  form triangles.",
    paste("Reciprocity - 1:", metrics$reciprocity_def),
      "  Ratio of num of dyads with reciprocated (mutual)\n  edges to num of dyads with single edge.",
    paste("Reciprocity - 2:", metrics$reciprocity_ratio),
      "  Ratio of total num of reciprocated edges to\n  total num of edges.", "")

  centrlzn <- fmt_double_values(get_graph_centrlzn(g))
  out <- c(out, "Graph Centralization", "", paste("Degree:", centrlzn$degree))
  if (metrics$directed) {
    out <- c(out, c(
      paste("Indegree:", centrlzn$indegree),
      paste("Outdegree:", centrlzn$outdegree)))
  }
  out <- c(out, c(
    paste("Betweenness centralization:", centrlzn$betweenness),
    paste("Closeness centralization:", centrlzn$closeness)))

  paste0(out, collapse = "\n")
})
