# on change layout event
observeEvent(input$graph_layout_select, {
  shinyjs::reset("igraph_spread_slider") # reset graph spread when a new layout is selected
  
  if (input$graph_layout_select == "Graphopt") { 
    shinyjs::reset("graph_charge")
    shinyjs::reset("graph_mass")
    shinyjs::reset("graph_spr_len")
    shinyjs::reset("graph_spr_const")
  }
})

# generate a new random seed
observeEvent(input$graph_reseed_btn, g_rv$seed <- sample(1:20000, 1))

# update seed input
observeEvent(g_rv$seed, updateNumericInput(session, "graph_seed_input", value = g_rv$seed))

# set seed value
observeEvent(input$graph_seed_set_btn, {
  req(input$graph_seed_input)
  if (is.numeric(input$graph_seed_input) && (input$graph_seed_input > 0) && !is.infinite(input$graph_seed_input)) {
    g_rv$seed <- round(input$graph_seed_input, digits = 0)
  }
})
