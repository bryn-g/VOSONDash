#' VOSON Dashboard networkMetricsServer
#'
#' Network metrics and line plots.
#'

#### values ---------------------------------------------------------------------------------------------------------- #
#### events ---------------------------------------------------------------------------------------------------------- #
#### output ---------------------------------------------------------------------------------------------------------- #

output$network_metrics_details_output <-
  renderText({
    networkMetricsDetailsOutput()
  })

#### reactives ------------------------------------------------------------------------------------------------------- #

distrib_data <- reactive({
  g <- graphFilters()
  if (is.null(g)) {
    return(VOSONDash::emptyPlotMessage("No graph data."))
  }
  
  type <- input$metrics_distrib_sel
  
  if (type == "component") {
    if (is.null(g)) {
      return(VOSONDash::emptyPlotMessage("No graph data."))
    }
    
    cc <- components(g, mode = input$graph_component_type_select)
    plot(
      table(cc$csize),
      type = "b",
      xlab = paste0(
        "Size of component (",
        input$graph_component_type_select,
        ")"
      ),
      ylab = "N components"
    )
  } else if (type == "degree") {
    if (is.directed(g)) {
      VOSONDash::emptyPlotMessage("Not defined for network.")
    } else {
      plot(table(degree(g)),
           type = "b",
           xlab = "Degree",
           ylab = "N nodes")
    }
  } else if (type == "indegree") {
    if (is.directed(g)) {
      plot(table(degree(g, mode = "in")),
           type = "b",
           xlab = "Indegree",
           ylab = "N nodes")
    } else {
      VOSONDash::emptyPlotMessage("Not defined for undirected network.")
    }
  } else if (type == "outdegree") {
    if (is.directed(g)) {
      plot(table(degree(g, mode = "out")),
           type = "b",
           xlab = "Outdegree",
           ylab = "N nodes")
    } else {
      VOSONDash::emptyPlotMessage("Not defined for undirected network.")
    }
  } else {
    VOSONDash::emptyPlotMessage("No graph data.")
  }
})

output$metrics_distrib_plot <- renderPlot({
  distrib_data()
})

networkMetricsDetailsOutput <- reactive({
  g <- graphFilters()
  
  output <- c()
  
  if (!is.null(g)) {
    metrics <-
      getNetworkMetrics(g, component_type = input$graph_component_type_select)
    
    output <- append(
      output,
      c(
        paste("Number of nodes (network size):", metrics$nodes),
        paste("Number of edges:", metrics$edges),
        paste(
          "Edges:",
          ifelse(metrics$directed, "directed", "undirected")
        ),
        paste("Number of components:", metrics$components),
        paste("Component mode:", metrics$components_type),
        paste("Number of isolates:", metrics$isolates),
        paste("Density:", sprintf("%.3f", metrics$density)),
        paste(
          "Average geodesic distance:",
          sprintf("%.3f", metrics$ave_geodesic_dist)
        ),
        "",
        paste(
          "(Global) clustering coefficient:",
          sprintf("%.3f", metrics$global_clust_coeff)
        ),
        "  Proportion of connected triples that close to\n  form triangles.",
        paste(
          "Reciprocity - 1:",
          sprintf("%.3f", metrics$reciprocity_def)
        ),
        "  Ratio of number of dyads with reciprocated (mutual)\n  edges to number of dyads with single edge.",
        paste(
          "Reciprocity - 2:",
          sprintf("%.3f", metrics$reciprocity_ratio)
        ),
        "  Ratio of total number of reciprocated edges to\n  total number of edges.",
        ""
      )
    )
    
    if (metrics$directed) {
      output <- append(output, c(
        paste(
          "Indegree centralization:",
          sprintf("%.3f", metrics$indegree)
        ),
        paste(
          "Outdegree centralization:",
          sprintf("%.3f", metrics$outdegree)
        )
      ))
    } else{
      output <-
        append(output, paste(
          "Degree centralization:",
          sprintf("%.3f", metrics$degree)
        ))
    }
    
    output <- append(output, c(
      paste(
        "Betweenness centralization:",
        sprintf("%.3f", metrics$betweenness)
      ),
      paste(
        "Closeness centralization:",
        sprintf("%.3f", metrics$closeness)
      )
    ))
  } else {
    output <- append(output, "No graph data.")
  }
  
  paste0(output, collapse = '\n')
})
