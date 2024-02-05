# selected category updates select box with its attribute values
observeEvent(input$graph_cat_select, {
  graph_rv$graph_cat_selected <- input$graph_cat_select
  
  if (!is.null(r_graph_base())) {
    attr_choices <- c("All")
    
    if (input$graph_cat_select != "All") {
      attr_choices <- append(attr_choices, graph_rv$graph_cats[[input$graph_cat_select]])
    }
    
    # update list of values in select box
    updateSelectInput(session, "graph_sub_cats_select", choices = attr_choices, selected = "All")
    
    # enable select box control
    shinyjs::enable("graph_sub_cats_select")
  }
}, ignoreInit = TRUE)

# enable assortativity tab when a category other than "all" selected
observeEvent(graph_rv$graph_cat_selected, {
  if (graph_rv$graph_cat_selected %in% c("", "All")) {
    addCssClass(selector = "a[data-value = 'assort_tab_panel']", class = "inactive_menu_link")
  } else {
    removeCssClass(selector = "a[data-value = 'assort_tab_panel']", class = "inactive_menu_link")
  }
})
