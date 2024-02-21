assort_rv <- reactiveValues(
  mixmat_msg = NULL # displays a message if problem with mixing matrix
)

output$assort_details <- renderText({
  assort_txt()
})

output$mixmat <- DT::renderDataTable({
  DT::datatable(mixmat_txt(), options = list(paging = FALSE, searching = FALSE, bInfo = FALSE, ordering = FALSE))
})

output$homophily_details <- renderText({
  homophily_txt()
})

output$mixmat_details <- renderText({
  assort_rv$mixmat_msg
})

# returns selected categorical attribute output message
assort_txt <- reactive({
  g <- r_graph_filter()
  
  output <- c()
  
  if (!is.null(g)) {
    cat_sel <- g_nodes_rv$cat_selected
    if (nchar(cat_sel) && cat_sel != "All") {   # eventually will have cat attr selected by default...
      output <- append(output, paste0("Selected categorical attribute is: ", cat_sel))
      output <- append(output, "")
    } else { return(NULL) }
  } else { return(NULL) }
  
  paste0(output, collapse = "\n")
})

# creates and returns mixing matrix dataframe, or returns null and sets an output message
mixmat_txt <- reactive({
  g <- r_graph_filter()
  
  if (!isTruthy(g)) {
    assort_rv$mixmat_msg <- "No Data."
    return(NULL)
  }
  
  cat_sel <- g_nodes_rv$cat_selected
  if (nchar(cat_sel) && cat_sel != "All") {  # eventually will have cat attr selected by default...
    assort_rv$mixmat_msg <- NULL
    df <- VOSONDash::mixmat(g, cat_sel, use_density = FALSE)
    return(df)
  } else {
    assort_rv$mixmat_msg <- "Categorical attribute not present, or not selected."
    return(NULL)      
  }
})

# returns output for homophily index calculations
homophily_txt <- reactive({
  g <- r_graph_filter()
  
  output <- c()
  
  if (!is.null(g)) {
    cat_sel <- g_nodes_rv$cat_selected
    if (nchar(cat_sel) && cat_sel != "All") {   # eventually will have cat attr selected by default...
      # vattr <- paste0("vosonCA_", cat_sel)
      # mm <- VOSONDash::mixmat(g, paste0("vosonCA_", cat_sel), use_density = FALSE)
      mm <- VOSONDash::mixmat(g, cat_sel, use_density = FALSE)
      
      # attr_list <- g_nodes_rv$cats[[cat_sel]]
      attr_list <- f_get_cat_values(g_rv$attrs, cat_sel)
      
      # if subset of attributes selected
      if (input$cat_sub_sel[1] != "All") {
        attr_list <- input$cat_sub_sel
      }
      
      for (i in attr_list) {
        output <- append(output, paste0("Category: ", i))
        w_i <- length(which(igraph::vertex_attr(g, cat_sel) == i)) / length(igraph::V(g))
        output <- append(output, paste0("  Population share: ", sprintf("%.3f", w_i)))
        if (w_i > 0) {
          H_i <- mm[i, i] / rowSums(mm)[i]
          output <- append(output, paste0("  Homogeneity index: ", sprintf("%.3f", H_i)))
          Hstar_i <- (H_i - w_i) / (1 - w_i)
          output <- append(output, paste0("  Homophily index: ", sprintf("%.3f", Hstar_i)))          
        }
        output <- append(output, "")
      }
      output <- append(output, "")
    }else{
      output <- append(output, paste0("Categorical attribute not present, or not selected."))
    }
  }else{
    output <- append(output, paste0("No data."))
  }
  
  paste0(output, collapse = "\n")
})
