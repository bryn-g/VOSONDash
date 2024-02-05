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