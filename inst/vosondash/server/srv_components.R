c_rv <- reactiveValues(
  c_weak = list(n = 0, min = NULL, max = NULL),
  c_strong = list(n = 0, min = NULL, max = NULL)
)

output$component_summary_ui <- renderText({
  graphComponentSummary()
})

graphComponentSummary <- reactive({
  type <- input$graph_component_type_select
  n <-
    switch(type,
           "weak" = c_rv$c_weak$n,
           "strong" = c_rv$c_strong$n)
  min <-
    switch(type,
           "weak" = c_rv$c_weak$min,
           "strong" = c_rv$c_strong$min)
  max <-
    switch(type,
           "weak" = c_rv$c_weak$max,
           "strong" = c_rv$c_strong$max)
  
  output <- c()
  output <- append(output, paste0("Components (", type, "): ", n))
  if (n > 0) {
    if (n == 1) {
      output <- append(output, paste0("Size: ", min, sep = ""))
    } else {
      output <-
        append(output, paste0("Size min: ", min, " max: ", max, sep = ""))
    }
  } else {
    output <- append(output, paste0(""))
  }
  paste0(output, collapse = '\n')
})

output$component_count_ui <- renderText({
  graphComponentCount()
})

graphComponentCount <- reactive({
  g <- graphFilters()
  n <- 0
  if (!is.null(g)) {
    n <- components(g, mode = input$graph_component_type_select)$no
  }
  paste0("Selected: ", n)
})

observeEvent(input$graph_component_type_select, {
  setComponentSliders()
}, ignoreInit = TRUE)

observeEvent(input$graph_comps_recalculate, {
  setComponentSliders()
  updateCheckboxInput(session, inputId = "graph_components_check", value = FALSE)
}, ignoreInit = TRUE)

setComponentSliders <- function() {
  c_range <- c_rv$c_weak
  if (input$graph_component_type_select == "strong") {
    c_range <- c_rv$c_strong
  }
  
  updateSliderInput(
    session,
    inputId = "graph_component_slider",
    min = c_range$min,
    max = c_range$max,
    value = c(c_range$min, c_range$max)
  )
}

setComponentRanges <- function(g, c_type) {
  if (!is.null(g)) {
    c <- components(g, mode = "weak")
    c_rv$c_weak$n <- c$no
    c_rv$c_weak$min <- suppressWarnings(min(c$csize))
    c_rv$c_weak$max <- suppressWarnings(max(c$csize))
    
    c <- components(g, mode = "strong")
    c_rv$c_strong$n <- c$no
    c_rv$c_strong$min <- suppressWarnings(min(c$csize))
    c_rv$c_strong$max <- suppressWarnings(max(c$csize))
  }
}
