
c_weak_range <- list(min = NULL, max = NULL)
c_strong_range <- list(min = NULL, max = NULL)
c_sel_range <- list(min = NULL, max = NULL)

output$component_summary_ui <- renderText({
  graphComponentSummary()
})

graphComponentSummary <- reactive({
  g <- graphFilters()
  
  output <- c()
  
  if (!is.null(g)) {
    graph_clusters <- components(g, mode = isolate(input$graph_component_type_select))
    
    output <- append(output, paste0("Components (", isolate(input$graph_component_type_select), "): ", 
                                    graph_clusters$no))
    
    min_value <- max_value <- 0
    if (graph_clusters$no > 0) {
      # suppress no non-missing arguments to min; returning Inf warning
      min_value <- suppressWarnings(min(graph_clusters$csize))
      max_value <- suppressWarnings(max(graph_clusters$csize))
    }
    
    if (graph_clusters$no == 1) {
      output <- append(output, paste0("Size: ", min_value, sep = ""))
    } else {
      output <- append(output, paste0("Size min: ", min_value, " max: ", max_value, sep = ""))
    }
  }else {
    output <- append(output, paste0(""))
  }
  
  paste0(output, collapse = '\n')
})


updateComponentSlider <- function(g, component_type) {
  # note: not graphFilter
  if (!is.null(g)) {
    graph_clusters <- components(g, mode = component_type)
    
    # suppress no non-missing arguments to min; returning Inf warning
    min_cluster_size <- suppressWarnings(min(graph_clusters$csize))
    max_cluster_size <- suppressWarnings(max(graph_clusters$csize))
    
    # likely causes a double render when graph has a component max size greater than the initial slider max set in ui
    updateSliderInput(session, inputId = "graph_component_slider", min = min_cluster_size,
                      max = max_cluster_size, value = c(min_cluster_size, max_cluster_size))
    
    # i <- 0
    # grps <- sapply(graph_clusters$csize, function(x) {
    #   i <<- i + 1
    #   paste0(i, " (size:", x, ")")
    # })
    # values <- as.character(1:length(grps))
    # names(values) <- grps
    # grps <- append(c("None" = "None"), values)
    # updatePickerInput(
    #   session,
    #   inputId = "component_membership_select",
    #   label = NULL,
    #   choices = grps
    # )
    # updateSelectInput(session, inputId = "component_membership_select", choices = c("None", unique(graph_clusters$membership)))
  }
}

observeEvent(input$graph_component_type_select, {
  c_range <- c_weak_range
  if (input$graph_component_type_select == "strong") {
    c_range <- c_strong_range
  }
  
  updateSliderInput(session, inputId = "graph_component_slider", min = c_range$min,
                    max = c_range$max, value = c(c_range$min, c_range$max))
})

setComponentRanges <- function(g, c_type) {
  if (!is.null(g)) {
    c <- components(g, mode = "weak")
    c_weak_range$min <- suppressWarnings(min(c$csize))
    c_weak_range$max <- suppressWarnings(max(c$csize))

    c <- components(g, mode = "strong")
    c_strong_range$min <- suppressWarnings(min(c$csize))
    c_strong_range$max <- suppressWarnings(max(c$csize))      
  }
}

updateComponentSlider_ <- function(g, c_type) {
  if (!is.null(g)) {
    c <- components(g, mode = c_type)
    
    min_size <- suppressWarnings(min(c$csize))
    max_size <- suppressWarnings(max(c$csize))
    
    updateSliderInput(session, inputId = "graph_component_slider", min = min_size,
                      max = max_size, value = c(min_size, max_size))
  }
}
